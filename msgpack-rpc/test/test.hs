{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (forConcurrently, race_)
import           Control.Concurrent.MVar    (newMVar)

import           Control.Monad              (forM_)
import           Control.Monad.Trans        (liftIO)

import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Network                    (withSocketsDo)
import           Network.MessagePack.Client (Client, ConnectionMap, call,
                                             createMVarLRU, execClient,
                                             execClientWithMap)
import           Network.MessagePack.Server (Server, method, serve)

main :: IO ()
main = withSocketsDo $ defaultMain $
  testGroup "simple service"
  [ testCase "test_simple"      $ server `race_` (threadDelay 1000 >> client 123)
  , testCase "test_sequence"    $ server `race_` (threadDelay 1000 >> clientsInSequence 10)
  , testCase "test_async_small" $ server `race_` (threadDelay 1000 >> concurrentClients 2)
  , testCase "test_async_big"   $ server `race_` (threadDelay 1000 >> concurrentClients 10)
  , testCase "test_map_small"   $ server `race_` (threadDelay 1000 >> twoClientsWithMap)
  , testCase "test_map_async"   $ server `race_` (threadDelay 1000 >> concurrentClientsWithMap 10)
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

simpleClientActions :: Int -> Client ()
simpleClientActions n = do
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
client n = execClient "127.0.0.1" port $ simpleClientActions n

clientWithMap :: ConnectionMap -> Int -> IO ()
clientWithMap lruMapVar n
  = execClientWithMap lruMapVar "127.0.0.1" port $ simpleClientActions n

clientsInSequence :: Int -> IO ()
clientsInSequence size = forM_ [1 .. size] $ \n -> client n

concurrentClients :: Int -> IO ()
concurrentClients size = do
  let tests = [1 .. size]
  () <$ forConcurrently tests client

twoClientsWithMap :: IO ()
twoClientsWithMap = do
  lruMapVar <- createMVarLRU 2
  clientWithMap lruMapVar 1
  clientWithMap lruMapVar 2

concurrentClientsWithMap :: Int -> IO ()
concurrentClientsWithMap size = do
  lruMapVar <- createMVarLRU $ toInteger (size `div` 2)
  let tests  = [1 .. size]
  () <$ forConcurrently tests (clientWithMap lruMapVar)
