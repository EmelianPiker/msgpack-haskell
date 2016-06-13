{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
-- License   : BSD3
--
-- Maintainer:  Hideyuki Tanaka <tanaka.hideyuki@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module is client library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Client
-- >
-- > add :: Int -> Int -> Client Int
-- > add = call "add"
-- >
-- > main = execClient "localhost" 5000 $ do
-- >   ret <- add 123 456
-- >   liftIO $ print ret
--
--------------------------------------------------------------------

module Network.MessagePack.Client (
  -- * MessagePack Client type
  Connection,
  ConnectionMap,
  Client,
  createMVarLRU,
  clearLRUMap,
  execClient,
  execClientWithMap,

  -- * Call RPC method
  call,

  -- * RPC error
  RpcError(..),
  ) where

import           Control.Concurrent.MVar           (MVar, newMVar, putMVar,
                                                    takeMVar)
import           Control.Exception                 (Exception, bracket)
import           Control.Monad                     (forM_, when)
import           Control.Monad.Catch               (MonadThrow (..))
import           Control.Monad.Extra               (whenJust)
import           Control.Monad.Reader              (MonadReader, ReaderT, ask,
                                                    runReaderT)
import           Control.Monad.Trans               (MonadIO, liftIO)

import           Data.Binary                       as Binary
import qualified Data.ByteString                   as S
import           Data.Cache.LRU                    (LRU)
import qualified Data.Cache.LRU                    as LRU
import           Data.Conduit                      (ResumableSource, Sink, ($$),
                                                    ($$+), ($$++))
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network              (appSink, appSource,
                                                    clientSettings)
import           Data.Conduit.Serialization.Binary (sinkGet)
import           Data.MessagePack                  (MessagePack (fromObject, toObject),
                                                    Object, pack)
import           Data.Typeable                     (Typeable)

import           Data.Streaming.Network            (getSocketFamilyTCP,
                                                    safeRecv)
import           Data.Streaming.Network.Internal   (AppData (appCloseConnection'))
import           Data.Streaming.Network.Internal   (AppData (..),
                                                    ClientSettings (..))
import qualified Network.Socket                    as NS
import           Network.Socket.ByteString         (sendAll)

-- | RPC connection type
data Connection
  = Connection
    !(ResumableSource IO S.ByteString)
    !(Sink S.ByteString IO ())
    !Int

type RemoteKey      = (S.ByteString, Int)
type ConnectionData = (AppData, Connection)
type ConnectionMap  = MVar (LRU RemoteKey ConnectionData)

data ClientState = ClientState
  { refMap     :: ConnectionMap
  , remoteAddr :: RemoteKey
  }

newtype Client a = Client
  { runClient :: ReaderT ClientState IO a
  } deriving (Functor, Applicative, Monad, MonadIO,
              MonadReader ClientState, MonadThrow)

runTCPClientUnclose :: ClientSettings -> (AppData -> IO a) -> IO a
runTCPClientUnclose (ClientSettings port host addrFamily readBufferSize) app = bracket
   (getSocketFamilyTCP host port addrFamily)
   -- (NS.sClose . fst)
   (const $ return ())
   (\(s, address) -> app AppData
       { appRead' = safeRecv s readBufferSize
       , appWrite' = sendAll s
       , appSockAddr' = address
       , appLocalAddr' = Nothing
       , appCloseConnection' = NS.sClose s
       , appRawSocket' = Just s
       })

createMVarLRU :: Integer -> IO ConnectionMap
createMVarLRU = newMVar . LRU.newLRU . Just

clearLRUMap :: ConnectionMap -> IO ()
clearLRUMap lruMapVar = do
  lruMap <- takeMVar lruMapVar

  let lruSize = LRU.maxSize lruMap
  let connections = map (fst . snd) $ LRU.toList lruMap
  forM_ connections appCloseConnection'

  putMVar lruMapVar $ LRU.newLRU lruSize

execClient :: S.ByteString -> Int -> Client a -> IO ()
execClient host port client = do
  singleMap <- createMVarLRU 1
  execClientWithMap singleMap host port client

execClientWithMap :: ConnectionMap -> S.ByteString -> Int -> Client a -> IO ()
execClientWithMap lruMapVar host port m = do
  lruMap <- takeMVar lruMapVar
  let keyPair = (host, port)
  let (updatedMap, mData) = LRU.lookup (host, port) lruMap

  case mData of
    Just _  -> putMVar lruMapVar updatedMap
    Nothing -> runTCPClientUnclose (clientSettings port host) $ \appData -> do
      (rsrc, _) <- appSource appData $$+ return ()

      let newConnection = Connection rsrc (appSink appData) 0
      let newClientData = (appData, newConnection)
      let (newMap, oldConn) = LRU.insertInforming keyPair newClientData updatedMap

      whenJust oldConn $ \(_, (oldData, _)) -> appCloseConnection' oldData

      putMVar lruMapVar newMap

  () <$ runReaderT (runClient m) (ClientState lruMapVar keyPair)

-- | RPC error type
data RpcError
  = ServerError Object     -- ^ Server error
  | ResultTypeError String -- ^ Result type mismatch
  | ProtocolError String   -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError

class RpcType r where
  rpcc :: String -> [Object] -> r

instance MessagePack o => RpcType (Client o) where
  rpcc m args = do
    res <- rpcCall m (reverse args)
    case fromObject res of
      Just r  -> return r
      Nothing -> throwM $ ResultTypeError "type mismatch"

instance (MessagePack o, RpcType r) => RpcType (o -> r) where
  rpcc m args arg = rpcc m (toObject arg:args)

rpcCall :: String -> [Object] -> Client Object
rpcCall methodName args = do
  ClientState {..} <- ask
  lruMap <- liftIO $ takeMVar refMap
  let (touchedMap, Just (appData, Connection rsrc sink msgid)) = LRU.lookup remoteAddr lruMap

  (rsrc', res) <- liftIO $ do
    CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
    rsrc $$++ sinkGet Binary.get

  let updatedConnection = Connection rsrc' sink (msgid + 1)
  let updatedClientData = (appData, updatedConnection)
  let updatedMap        = LRU.insert remoteAddr updatedClientData touchedMap
  liftIO $ putMVar refMap updatedMap

  case fromObject res of
    Nothing -> throwM $ ProtocolError "invalid response data"
    Just (rtype, rmsgid, rerror, rresult) -> do

      when (rtype /= (1 :: Int)) $
        throwM $ ProtocolError $
          "invalid response type (expect 1, but got " ++ show rtype ++ ")"

      when (rmsgid /= msgid) $
        throwM $ ProtocolError $
          "message id mismatch: expect "
          ++ show msgid ++ ", but got "
          ++ show rmsgid

      case fromObject rerror of
        Nothing -> throwM $ ServerError rerror
        Just () -> return rresult

-- | Call an RPC Method
call :: RpcType a
        => String -- ^ Method name
        -> a
call m = rpcc m []
