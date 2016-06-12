{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (forConcurrently, race_)
import           Control.Concurrent.MVar    (newMVar)
import           Control.Monad.Trans        (liftIO)

import qualified Data.HashMap.Strict        as HM

import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Network                    (withSocketsDo)
import           Network.MessagePack.Client (Client, IOMapVar, call, execClient,
                                             execClientWithMap)
import           Network.MessagePack.Server (Server, method, serve)

main :: IO ()
main = withSocketsDo $ defaultMain $
  testGroup "simple service"
  [ testCase "test_simple"      $ server `race_` (threadDelay 1000 >> client 123)
  , testCase "test_async_small" $ server `race_` (threadDelay 1000 >> concurrentClients 2)
  , testCase "test_async_big"   $ server `race_` (threadDelay 1000 >> concurrentClients 1000)
  , testCase "test_map_small"   $ server `race_` (threadDelay 1000 >> twoClientsWithMap)
  , testCase "test_map_async"   $ server `race_` (threadDelay 1000 >> concurrentClientsWithMap 2)
  ]

port :: Int
port = 5000

server :: IO ()
server =
  serve port
    [ method "add"  add
    , method "echo" echo
    ]
  where
    add :: Int -> Int -> Server Int
    add x y = return $ x + y

    echo :: String -> Server String
    echo s = return $ "***" ++ s ++ "***"

simpleClientAtions :: Int -> Client ()
simpleClientAtions n = do
  r1 <- add n n
  liftIO $ r1 @?= n + n
  r2 <- echo $ "hello" ++ show n
  liftIO $ r2 @?= "***hello" ++ show n ++ "***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

client :: Int -> IO ()
client n = execClient "127.0.0.1" port $ simpleClientAtions n

clientWithMap :: IOMapVar -> Int -> IO ()
clientWithMap hashMapVar n
  = execClientWithMap hashMapVar "127.0.0.1" port $ simpleClientAtions n

concurrentClients :: Int -> IO ()
concurrentClients size = do
  let tests = [1 .. size]
  () <$ forConcurrently tests client

twoClientsWithMap :: IO ()
twoClientsWithMap = do
  hashMapVar <- newMVar HM.empty
  clientWithMap hashMapVar 1
  clientWithMap hashMapVar 2

concurrentClientsWithMap :: Int -> IO ()
concurrentClientsWithMap size = do
  hashMapVar <- newMVar HM.empty
  let tests   = [1 .. size]
  () <$ forConcurrently tests (clientWithMap hashMapVar)