{-# LANGUAGE 
    MagicHash 
  , TemplateHaskell
  , MultiWayIf
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
  [ assertBoolTest "f"         $ isF         $ fromAsciiString "f"
  , assertBoolTest "fo"        $ isFo        $ fromAsciiString "fo"
  , assertBoolTest "foo"       $ isFoo       $ fromAsciiString "foo"
  , assertBoolTest "foob"      $ isFoob      $ fromAsciiString "foob"
  , assertBoolTest "fooba"     $ isFooba     $ fromAsciiString "fooba"
  , assertBoolTest "foobar"    $ isFoobar    $ fromAsciiString "foobar"
  , assertBoolTest "foobarb"   $ isFoobarb   $ fromAsciiString "foobarb"
  , assertBoolTest "foobarba"  $ isFoobarba  $ fromAsciiString "foobarba"
  , assertBoolTest "foobarbaz" $ isFoobarbaz $ fromAsciiString "foobarbaz"
  ]
  where
  assertBoolTest x b = testCase x $ assertBool x b
