{-# LANGUAGE
    TemplateHaskell
  , ViewPatterns
  , BangPatterns
#-}

module Data.Bytes.Patterns 
  ( makeBytesPatterns
  ) where

import Language.Haskell.TH
import Data.Word (Word8)
import Data.Char (ord, toUpper, toLower)
import GHC.Exts (Ptr(Ptr))
import Data.Foldable
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Types as BytesT

makeBytesPatterns :: [String] -> Q [Dec]
makeBytesPatterns bs = do
  xs <- traverse makeBytesPattern bs
  pure $ mconcat xs

makeBytesPattern :: String -> Q [Dec]
makeBytesPattern s = do
  name <- newName $ (toUpper $ head s) : tail s
  fnName <- newName $ ((toLower $ head s) : tail s) <> "Pattern"
  pure $ 
    [ PragmaD $ InlineP fnName Inline FunLike AllPhases
    , SigD fnName $ ArrowT `AppT` ConT ''Bytes.Bytes `AppT` ConT ''Bool
    , FunD fnName [Clause [VarP x] (NormalB expr) []]
    , PatSynSigD name (ConT ''Bytes.Bytes)
    , PatSynD name (PrefixPatSyn []) Unidir $ ViewP (VarE fnName) (ConP 'True [])
    ]
  where
  x :: Name
  x = mkName "x"
  bytes@(BytesT.Bytes _ _ len) = Bytes.fromLatinString s
  expr :: Exp
  expr = 
        (     (ParensE $ (LitE $ IntegerL $ fromIntegral len))
          === (ParensE $ VarE 'Bytes.length `AppE` VarE x)
        )
    &&& (     (ParensE $ (LitE $ IntegerL $ fromIntegral $ Bytes.fnv1a64 bytes))
          === (ParensE $ VarE 'Bytes.fnv1a64 `AppE` VarE x) 
        )
    &&& ParensE ((equalsN len s) `AppE` VarE x)

equalsN :: Int -> String -> Exp
equalsN len s = case len of
    1 -> unroll s $ VarE 'Bytes.equalsLatin1
    2 -> unroll s $ VarE 'Bytes.equalsLatin2
    3 -> unroll s $ VarE 'Bytes.equalsLatin3
    4 -> unroll s $ VarE 'Bytes.equalsLatin4
    5 -> unroll s $ VarE 'Bytes.equalsLatin5
    6 -> unroll s $ VarE 'Bytes.equalsLatin6
    7 -> unroll s $ VarE 'Bytes.equalsLatin7
    8 -> unroll s $ VarE 'Bytes.equalsLatin8
    _ -> VarE 'Bytes.equalsCString `AppE` (ConE 'Ptr `AppE` cstring s)
  where
  unroll [] _ = error "bug in `bytes-patterns`: unroll"
  unroll [c] e = e `AppE` (LitE $ CharL c)
  unroll (x:xs) e = foldl' (\acc c -> acc `AppE` (LitE $ CharL c)) (e `AppE` (LitE $ CharL x)) xs
  cstring x = LitE $ StringPrimL $ fmap c2w x
  
-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

{-# INLINE (&&&) #-}
(&&&) :: Exp -> Exp -> Exp
a &&& b = InfixE
  (Just $ ParensE a)
  (VarE '(&&))
  (Just $ ParensE b)

{-# INLINE (===) #-}
(===) :: Exp -> Exp -> Exp
a === b = InfixE
  (Just $ ParensE a)
  (VarE '(==))
  (Just $ ParensE b)
