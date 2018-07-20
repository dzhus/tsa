{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import Data.Maybe
import Data.Monoid

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Regression as S

type Series e = V.Vector e

diff :: (Num e, V.Unbox e) => Int -> Series e -> Series e
diff n series = V.zipWith (-) (V.drop n series) series

-- | Model is a forecasting function.
--
-- If the argument is Nothing, predict existing values.
type Model e = Maybe Int -> Series e -> Series e
{-@ type Model' e = Maybe Nat -> Series e -> Series e @-}

-- | Predict N next points for a time series.
{-@ forecast :: Nat -> Model' e -> Series e -> Series e @-}
forecast :: Int -> Model e -> Series e -> Series e
forecast periods f = f $ Just periods

{-@ extendBy :: Nat -> Model' e -> Series e -> Series e @-}
extendBy :: V.Unbox e => Int -> Model e -> Series e -> Series e
extendBy periods f s = s <> forecast periods f s

{-@ mean :: Model' Double @-}
mean :: Model Double
mean n s = V.replicate n' $ S.mean s
  where
    n' = fromMaybe (V.length s) n

{-@ linearTrend :: Model' Double @-}
linearTrend :: Model Double
linearTrend n s = V.iterateN n' (+ slope) start
  where
    n' = fromMaybe len n
    len = V.length s
    (coeffs, _) = S.olsRegress [V.generate len fromIntegral] s
    (slope, bias) = (coeffs V.! 0, coeffs V.! 1)
    start = fromIntegral (maybe 0 (const len) n) * slope + bias

residuals :: (Num e, V.Unbox e) => Model e -> Series e -> Series e
residuals f s = V.zipWith (-) (f Nothing s) s
