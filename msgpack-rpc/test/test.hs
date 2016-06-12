{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (forConcurrently, race_)
import           Control.Monad.Trans        (liftIO)
import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Network                    (withSocketsDo)
import           Network.MessagePack.Client (Client, call, execClient)
import           Network.MessagePack.Server (Server, method, serve)

main :: IO ()
main = withSocketsDo $ defaultMain $
  testGroup "simple service"
  [ testCase "test_simple"      $ server `race_` (threadDelay 1000 >> client)
  , testCase "test_async_small" $ server `race_` (threadDelay 1000 >> concurrentClients 2)
  , testCase "test_async_big"   $ server `race_` (threadDelay 1000 >> concurrentClients 1000)
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

client :: IO ()
client = customClient 123

customClient :: Int -> IO ()
customClient number = execClient "127.0.0.1" port $ do
  r1 <- add number number
  liftIO $ r1 @?= number + number
  r2 <- echo $ "hello" ++ show number
  liftIO $ r2 @?= "***hello" ++ show number ++ "***"
  where
    add :: Int -> Int -> Client Int
    add = call "add"

    echo :: String -> Client String
    echo = call "echo"

concurrentClients :: Int -> IO ()
concurrentClients size = do
  let tests = [1 .. size]
  () <$ forConcurrently tests customClient

