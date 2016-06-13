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
  IOMapVar,
  Client,
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
import           Control.Monad                     (when)
import           Control.Monad.Catch               (MonadThrow (..))
import           Control.Monad.Reader              (MonadReader, ReaderT, ask,
                                                    runReaderT)
import           Control.Monad.Trans               (MonadIO, liftIO)

import           Data.Binary                       as Binary
import qualified Data.ByteString                   as S
import           Data.Conduit                      (ResumableSource, Sink, ($$),
                                                    ($$+), ($$++))
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network              (appSink, appSource,
                                                    clientSettings,
                                                    runTCPClient)
import           Data.Conduit.Serialization.Binary (sinkGet)
import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HM
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

type IOMapVar    = MVar (HashMap (S.ByteString, Int) Connection)
data ClientState = ClientState
  { refMap     :: IOMapVar
  , clientAddr :: (S.ByteString, Int)
  }

newtype Client a
  = ClientT { runClient :: ReaderT ClientState IO a }
  deriving (Functor, Applicative, Monad, MonadIO,
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
       , appCloseConnection' = return ()
       , appRawSocket' = Just s
       })

execClient :: S.ByteString -> Int -> Client a -> IO ()
execClient host port client = do
  emptyMap <- newMVar HM.empty
  execClientWithMap emptyMap host port client

execClientWithMap :: IOMapVar -> S.ByteString -> Int -> Client a -> IO ()
execClientWithMap hashMapVar host port m = do
  hashMap    <- takeMVar hashMapVar
  let keyPair = (host, port)
  let mConn   = HM.lookup (host, port) hashMap

  runTCPClientUnclose (clientSettings port host) $ \appData -> do
    let unclosableAppData = appData { appCloseConnection' = return () }
    case mConn of
      Nothing -> do
        (rsrc, _) <- appSource unclosableAppData $$+ return ()
        let newConnection = Connection rsrc (appSink unclosableAppData) 0
        let newMap        = HM.insert keyPair newConnection hashMap
        putMVar hashMapVar newMap
      Just _  ->
        putMVar hashMapVar hashMap

    () <$ runReaderT (runClient m) (ClientState hashMapVar keyPair)

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
  ClientState hashMapVar myAddr <- ask
  hashMap <- liftIO $ takeMVar hashMapVar
  let Just (Connection rsrc sink msgid) = HM.lookup myAddr hashMap

  (rsrc', res) <- liftIO $ do
    CB.sourceLbs (pack (0 :: Int, msgid, methodName, args)) $$ sink
    rsrc $$++ sinkGet Binary.get

  let updatedConnection = Connection rsrc' sink (msgid + 1)
  let updatedMap        = HM.insert myAddr updatedConnection hashMap
  liftIO $ putMVar hashMapVar updatedMap

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
