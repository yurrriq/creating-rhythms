{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

import Data.Finite (finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Rhythm.Binary.FoldSequences (foldSequence)
import Data.Type.Equality (type (:~:) (..))
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal, type (^))
import Options.Applicative
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  (nInt, mInt, f) <- customExecParser p opts
  case (someNatVal (fromIntegral nInt), someNatVal (fromIntegral mInt)) of
    (SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy m)) ->
      withKnownNatPower2 @m $ \(_ :: Proxy pow2m) ->
        putStrLn $
          concatMap (show . getFinite) $
            foldSequence @n @m (finite @pow2m f)
  where
    opts = info args (fullDesc <> progDesc "Generate fold sequences.")
    p = prefs showHelpOnEmpty

args :: Parser (Int, Integer, Integer)
args =
  (,,)
    <$> argument auto (metavar "N" <> help "the number of terms")
    <*> argument auto (metavar "M" <> help "number of bits")
    <*> argument auto (metavar "F" <> help "function number 0 -> 2^m-1")

withKnownNatPower2 ::
  forall m r.
  (KnownNat m) =>
  (forall pow2m. (KnownNat pow2m, pow2m ~ (2 ^ m)) => Proxy pow2m -> r) ->
  r
withKnownNatPower2 f =
  case someNatVal (2 ^ natVal (Proxy @m)) of
    SomeNat (proxyPow2m :: Proxy pow2m) ->
      case unsafeCoerce Refl :: pow2m :~: (2 ^ m) of
        Refl -> f proxyPow2m
