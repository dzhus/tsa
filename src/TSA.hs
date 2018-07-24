{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import Control.Lens
import Data.Maybe
import Data.Monoid

import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as S
import qualified Statistics.Regression as S

import Data.Colour.Names
import Graphics.Rendering.Chart.Easy as Chart

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

plotWithRadius :: Double -> EC (Layout x y) (PlotPoints x y) -> EC (Layout x y) ()
plotWithRadius radius p = Chart.plot $ do
  p' <- p
  return (p' & plot_points_style . point_radius .~ radius)

plotPredictions :: (PlotValue e, V.Unbox e)
                => Int
                -- ^ Forecast that many future points.
                -> Model e
                -> Series e
                -> Renderable ()
plotPredictions periods f s = toRenderable $ do
  Chart.setColors [opaque blue, red `withOpacity` 50, opaque red]
  mapM (plotWithRadius 5)
    [ points "Series" (zip [(0::Int)..] (V.toList s))
    , points "Predictions" $ zip [(0::Int)..] (V.toList $ f Nothing s)
    , points "Forecast" $ zip [V.length s..] (V.toList $ forecast periods f s)
    ]
