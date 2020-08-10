{-# LANGUAGE 
    MagicHash 
  , TemplateHaskell
  , PatternSynonyms
  , ViewPatterns
#-}

module Main where

import Data.Bytes
import Data.Bytes.Patterns
import Test.Tasty
import Test.Tasty.HUnit

makeBytesPatterns 
  [ "f"
  , "fo"
  , "foo"
  , "foob"
  , "fooba"
  , "foobar"
  , "foobarb"
  , "foobarba"
  , "foobarbaz"
  ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "f" $ case fromAsciiString "f" of
      F -> pure ()
      _ -> assertFailure "f"
  , testCase "fo" $ case fromAsciiString "fo" of
      Fo -> pure ()
      _ -> assertFailure "fo"
  , testCase "foo" $ case fromAsciiString "foo" of
      Foo -> pure ()
      _ -> assertFailure "foo"
  , testCase "foob" $ case fromAsciiString "foob" of
      Foob -> pure ()
      _ -> assertFailure "foob"
  , testCase "fooba" $ case fromAsciiString "fooba" of
      Fooba -> pure ()
      _ -> assertFailure "fooba"
  , testCase "foobar" $ case fromAsciiString "foobar" of
      Foobar -> pure ()
      _ -> assertFailure "foobar"
  , testCase "foobarb" $ case fromAsciiString "foobarb" of
      Foobarb -> pure ()
      _ -> assertFailure "foobarb"
  , testCase "foobarba" $ case fromAsciiString "foobarba" of
      Foobarba -> pure ()
      _ -> assertFailure "foobarba"
  , testCase "foobarbaz" $ case fromAsciiString "foobarbaz" of
      Foobarbaz -> pure ()
      _ -> assertFailure "foobarbaz"
  ]


