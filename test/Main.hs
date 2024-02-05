{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Bytes.Patterns
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Bytes.Text.Ascii as Ascii

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
unitTests =
  testGroup
    "Unit tests"
    [ assertBoolTest "f" $ isF $ Ascii.fromString "f"
    , assertBoolTest "fo" $ isFo $ Ascii.fromString "fo"
    , assertBoolTest "foo" $ isFoo $ Ascii.fromString "foo"
    , assertBoolTest "foob" $ isFoob $ Ascii.fromString "foob"
    , assertBoolTest "fooba" $ isFooba $ Ascii.fromString "fooba"
    , assertBoolTest "foobar" $ isFoobar $ Ascii.fromString "foobar"
    , assertBoolTest "foobarb" $ isFoobarb $ Ascii.fromString "foobarb"
    , assertBoolTest "foobarba" $ isFoobarba $ Ascii.fromString "foobarba"
    , assertBoolTest "foobarbaz" $ isFoobarbaz $ Ascii.fromString "foobarbaz"
    ]
 where
  assertBoolTest x b = testCase x $ assertBool x b
