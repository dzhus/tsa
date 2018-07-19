{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Regression as S

type Series e = V.Vector e

diff :: (Num e, V.Unbox e) => Int -> Series e -> Series e
diff n series = V.zipWith (-) (V.drop n series) series

-- | Model is a forecasting function.
type Model e = Int -> Series e -> Series e

forecast :: Int -> Model e -> Series e -> Series e
forecast periods f = f periods

extendBy :: V.Unbox e => Int -> Model e -> Series e -> Series e
extendBy periods f s = s <> forecast periods f s

mean :: Model Double
mean n s = V.replicate n $ S.mean s

linearTrend :: Model Double
linearTrend n s =
  if V.length coeffs == 2
  then
    let
      (slope, bias) = (coeffs V.! 0, coeffs V.! 1)
    in
      V.iterateN n (+ slope) (fromIntegral len * slope + bias)
  else error "linearTrend: unexpected number of regression coefficients"
  where
    len = V.length s
    (coeffs, _) = S.olsRegress [V.generate len fromIntegral] s
