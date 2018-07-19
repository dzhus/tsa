{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S

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
