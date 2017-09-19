{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Test.Hspec (it, hspec, describe, shouldSatisfy, shouldBe, Spec)
import Test.Hspec.Core.Runner (hspecResult, Summary(..))
import Test.QuickCheck
import Data.Aeson
import GHCJS.Marshal
import Control.Monad
import Test.QuickCheck.Instances

import Miso
import Miso.FFI

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number <$> arbitrary
    string = String <$> arbitrary
    array = Array <$> arbitrary
    object' = Object <$> arbitrary


main :: IO ()
main = do
  Summary { summaryFailures } <- hspecResult tests
  phantomExit summaryFailures

tests :: Spec
tests = do
  storageTests

storageTests :: Spec
storageTests = describe "Storage tests" $ do
  it "should write to and read from local storage" $ do
    let obj = object [ "foo" .= ("bar" :: String) ]
    setLocalStorage "foo" obj
    Right r <- getLocalStorage "foo"
    r `shouldBe` obj
  it "should write to and read from session storage" $ do
    let obj = object [ "foo" .= ("bar" :: String) ]
    setSessionStorage "foo" obj
    Right r <- getLocalStorage "foo"
    r `shouldBe` obj
  it "Should round trip" $ do
    property $ \x -> do
      Just v <- jsvalToValue =<< toJSVal x
      v `shouldBe` x

phantomExit :: Int -> IO ()
phantomExit x
  | x <= 0 = phantomExitSuccess
  | otherwise = phantomExitFail

foreign import javascript unsafe "phantom.exit(0);"
  phantomExitSuccess :: IO ()

foreign import javascript unsafe "phantom.exit(1);"
  phantomExitFail :: IO ()
