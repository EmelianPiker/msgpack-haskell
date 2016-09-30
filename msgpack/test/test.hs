{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Maybe
import           Data.MessagePack
import qualified Data.Text                  as T
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

instance Arbitrary a => Arbitrary (Assoc a) where
  arbitrary = Assoc <$> arbitrary

instance Arbitrary S.ByteString where
  arbitrary = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack

mid' :: MessagePack a => a -> Maybe a
mid' = unpack . pack

tests :: TestTree
tests =
  testGroup "Identity Properties"
    [ testProperty "int" $
      \(a :: Int) -> a == mid a
    , testProperty "nil" $
      \(a :: ()) -> a == mid a
    , testProperty "bool" $
      \(a :: Bool) -> a == mid a
    , testProperty "double" $
      \(a :: Double) -> a == mid a
    , testProperty "string" $
      \(a :: String) -> a == mid a
    , testProperty "bytestring" $
      \(a :: S.ByteString) -> a == mid a
    , testProperty "lazy-bytestring" $
      \(a :: L.ByteString) -> a == mid a
    , testProperty "[int]" $
      \(a :: [Int]) -> a == mid a
    , testProperty "[string]" $
      \(a :: [String]) -> a == mid a
    , testProperty "(int, int)" $
      \(a :: (Int, Int)) -> a == mid a
    , testProperty "(int, int, int)" $
      \(a :: (Int, Int, Int)) -> a == mid a
    , testProperty "(int, int, int, int)" $
      \(a :: (Int, Int, Int, Int)) -> a == mid a
    , testProperty "(int, int, int, int, int)" $
      \(a :: (Int, Int, Int, Int, Int)) -> a == mid a
    , testProperty "[(int, double)]" $
      \(a :: [(Int, Double)]) -> a == mid a
    , testProperty "[(string, string)]" $
      \(a :: [(String, String)]) -> a == mid a
    , testProperty "Assoc [(string, int)]" $
      \(a :: Assoc [(String, Int)]) -> a == mid a
      -- maybe tests
    , testProperty "maybe int" $
      \(a :: Maybe Int) -> a == mid a
    , testProperty "maybe nil" $
      \(a :: Maybe ()) -> a == mid a

   -- FIXME: this test is also failing
   --
   -- it should probably be decoded somewhat specially with ObjectExt ?
   --
   -- , testProperty "maybe maybe int" $
   --   \(a :: Maybe (Maybe Int)) -> a == mid a
   --
   -- by looking at msgpack specification it looks like Haskells Maybe
   -- type should be probably decoded with custom ObjectExt
   --
    , testProperty "maybe unit" $
      \(a :: Maybe ()) -> a == mid a
    , testProperty "maybe bool" $
      \(a :: Maybe Bool) -> a == mid a
    , testProperty "maybe double" $
      \(a :: Maybe Double) -> a == mid a
    , testProperty "maybe string" $
      \(a :: Maybe String) -> a == mid a
    , testProperty "maybe bytestring" $
      \(a :: Maybe S.ByteString) -> a == mid a
    , testProperty "maybe lazy-bytestring" $
      \(a :: Maybe L.ByteString) -> a == mid a
    , testProperty "maybe [int]" $
      \(a :: Maybe [Int]) -> a == mid a
    , testProperty "maybe [string]" $
      \(a :: Maybe [String]) -> a == mid a
    , testProperty "maybe (int, int)" $
      \(a :: Maybe (Int, Int)) -> a == mid a
    , testProperty "maybe (int, int, int)" $
      \(a :: Maybe (Int, Int, Int)) -> a == mid a
    , testProperty "maybe (int, int, int, int)" $
      \(a :: Maybe (Int, Int, Int, Int)) -> a == mid a
    , testProperty "maybe (int, int, int, int, int)" $
      \(a :: Maybe (Int, Int, Int, Int, Int)) -> a == mid a
    , testProperty "maybe [(int, double)]" $
      \(a :: Maybe [(Int, Double)]) -> a == mid a
    , testProperty "maybe [(string, string)]" $
      \(a :: Maybe [(String, String)]) -> a == mid a
    , testProperty "maybe (Assoc [(string, int)])" $
      \(a :: Maybe (Assoc [(String, Int)])) -> a == mid a
      -- exception tests
    , testProperty "serializable exception fromObject behaviour" $
      \(a :: SerializableErrorBox ArrayException) ->
        mid' a == Nothing
    , testProperty "serializable exception fromObjectAsError behaviour" $
      \(a :: SerializableErrorBox ArrayException) ->
        fromObjectAsError (toObject a) == Just a
    , testProperty "non-serializable exception fromObject behaviour" $
      \(a :: NonSerializableError) ->
        mid' a == Nothing
     , testProperty "non-serializable exception fromObjectAsError behaviour" $
      \(a :: NonSerializableError) ->
        fromObjectAsError (toObject a) == Just a
    ]


-- guinea pigs

instance MessagePack ArrayException where
    toObject (IndexOutOfBounds s) = toObject (False, s)
    toObject (UndefinedElement s) = toObject (True, s)
    fromObject e = fromObject e >>=
        \(b, s) -> return $ if b
                            then UndefinedElement s
                            else IndexOutOfBounds s

instance Arbitrary ArrayException where
    arbitrary = arbitrary >>=
        \b -> if b
                 then IndexOutOfBounds <$> arbitrary
                 else UndefinedElement <$> arbitrary

instance Arbitrary e => Arbitrary (SerializableErrorBox e) where
    arbitrary = SerializableErrorBox <$> arbitrary

instance Arbitrary NonSerializableError where
    arbitrary = NonSerializableError . T.pack <$> arbitrary
