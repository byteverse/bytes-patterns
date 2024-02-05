{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Bytes.Patterns
  ( makeBytesPatterns
  ) where

import Data.Char (isSpace, ord, toUpper)
import Data.List
import Data.Word (Word8)
import GHC.Exts (Ptr (Ptr))
import Language.Haskell.TH

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.Bytes.Types as BytesT

data CheckHash
  = CheckHash
  | NoHash

{- | only use functions generated by this macro in the same case
generated functions follow the format "isFooBarBaz"
replacing spaces with camel case
-}
makeBytesPatterns :: [String] -> Q [Dec]
makeBytesPatterns bs = do
  let bsByLength = groupBy (\a b -> length a == length b) bs
  mconcat <$> traverse makeBytesPatternsEqualLen bsByLength

makeBytesPatternsEqualLen :: [String] -> Q [Dec]
makeBytesPatternsEqualLen bs = do
  let len = length bs
      checkHash = (if len < 3 then CheckHash else NoHash)
  mconcat <$> traverse (makeBytesPattern checkHash) bs

makeBytesPattern :: CheckHash -> String -> Q [Dec]
makeBytesPattern checkHash s = do
  -- name <- newName $ (toUpper $ head s) : tail s
  fnName <- newName $ "is" <> camelCase s
  pure
    [ PragmaD $ InlineP fnName Inline FunLike AllPhases
    , SigD fnName $ ArrowT `AppT` ConT ''Bytes.Bytes `AppT` ConT ''Bool
    , FunD fnName [Clause [VarP x] (NormalB expr) []]
    -- doesn't inline :^(
    -- , PatSynSigD name (ConT ''Bytes.Bytes)
    -- , PatSynD name (PrefixPatSyn []) Unidir $ ViewP (VarE fnName) (ConP 'True [])
    ]
 where
  x :: Name
  x = mkName "x"
  bytes@(BytesT.Bytes _ _ len) = Latin1.fromString s
  checkHashExp = case checkHash of
    CheckHash ->
      LitE (IntegerL $ fromIntegral $ Bytes.fnv1a64 bytes)
        === (VarE 'Bytes.fnv1a64 `AppE` VarE x)
    NoHash -> ConE 'True
  expr :: Exp
  expr =
    ( LitE (IntegerL $ fromIntegral len)
        === (VarE 'Bytes.length `AppE` VarE x)
    )
      &&& checkHashExp
      &&& ParensE (equalsN len s `AppE` VarE x)

equalsN :: Int -> String -> Exp
equalsN len s = case len of
  1 -> unroll s $ VarE 'Latin1.equals1
  2 -> unroll s $ VarE 'Latin1.equals2
  3 -> unroll s $ VarE 'Latin1.equals3
  4 -> unroll s $ VarE 'Latin1.equals4
  5 -> unroll s $ VarE 'Latin1.equals5
  6 -> unroll s $ VarE 'Latin1.equals6
  7 -> unroll s $ VarE 'Latin1.equals7
  8 -> unroll s $ VarE 'Latin1.equals8
  _ -> VarE 'Bytes.equalsCString `AppE` (ConE 'Ptr `AppE` cstring s)
 where
  unroll [] _ = error "bug in `bytes-patterns`: unroll"
  unroll [c] e = e `AppE` LitE (CharL c)
  unroll (x : xs) e = foldl' (\acc c -> acc `AppE` LitE (CharL c)) (e `AppE` LitE (CharL x)) xs
  cstring x = LitE $ StringPrimL $ fmap c2w x

{- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
silently truncates to 8 bits Chars > '\255'.
-}
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

{-# INLINE (&&&) #-}
(&&&) :: Exp -> Exp -> Exp
a &&& b =
  InfixE
    (Just $ ParensE a)
    (VarE '(&&))
    (Just $ ParensE b)

{-# INLINE (===) #-}
(===) :: Exp -> Exp -> Exp
a === b =
  InfixE
    (Just $ ParensE a)
    (VarE '(==))
    (Just $ ParensE b)

camelCase :: String -> String
camelCase = u . applyFirst toUpper
 where
  u [] = []
  u (x : xs)
    | isSpace x = toUpper x : u xs
    | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x : xs) = f x : xs
