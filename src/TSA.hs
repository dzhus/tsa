{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import Control.Monad.IO.Class
import Data.Attoparsec.ByteString.Char8
import Data.CSV.Conduit
import Data.Maybe
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid
#endif

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Statistics.Regression as S
import qualified Statistics.Sample as S

#ifdef WITH_IHASKELL
import Control.Lens
import Graphics.Rendering.Chart.Easy as Chart

import qualified Statistics.Autocorrelation as S
import qualified Statistics.Sample.Histogram as S
#endif

type Series e = V.Vector e

loadSeries :: MonadIO m
           => FilePath
           -- ^ CSV file name.
           -> Int
           -- ^ Index of column to use (1-based).
           -> m (Series Double)
loadSeries fname col = do
  r <- readCSVFile defCSVSettings fname
  return $ V.convert $ flip VB.mapMaybe r $
    \row -> (either (const Nothing) Just . parseOnly double) =<< row VB.!? col

diff :: (Num e, V.Unbox e)
     => Int
     -- ^ Differencing period.
     -> Series e
     -> Series e
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
residuals f s = V.zipWith (-) s (f Nothing s)

#ifdef WITH_IHASKELL
plotResiduals :: Model Double -> Series Double -> Renderable ()
plotResiduals f s = toRenderable $ do
  layout_title .= "Residuals"
  layout_legend .= Nothing
  Chart.plot $ plotBars <$> do
    b <- bars (map show $ V.toList sizes) (map (\(x, y) -> (x, [y :: Int])) barData)
    return $ b & plot_bars_spacing .~ BarsFixGap 0 5
  where
    barData = V.toList $ V.zip bins sizes
    -- TODO Customize bin count?
    (bins, sizes) = S.histogram 20 $ residuals f s

plotACF :: Int -> Series Double -> Renderable ()
plotACF maxLag s = toRenderable $ do
  layout_title .= "Autocorrelation"
  layout_legend .= Nothing
  layout_x_axis . laxis_title .= "Lag"
  -- Do not skip X values
  layout_x_axis . laxis_override .= (axis_labels .~ [[ (lag, show lag) | lag <- lags ]])
  Chart.plot $ plotBars <$>
    bars (map show lags) (map (\lag -> (lag, [acf V.! lag])) lags)
  where
    (acf, _, _) = S.autocorrelation s
    lags = [1 .. maxLag]

plotPredictions :: (PlotValue e, V.Unbox e)
                => Int
                -- ^ Forecast that many future points.
                -> Model e
                -> Series e
                -> Renderable ()
plotPredictions periods f s = toRenderable $ do
  Chart.setColors [opaque blue, red `withOpacity` 50, opaque red]
  Chart.plot $ line "Series" [zip [(0::Int)..] (V.toList s)]
  mapM_ (plotWithRadius 5)
    [ points "Predictions" $ zip [(0::Int)..] (V.toList $ f Nothing s)
    , points "Forecast" $ zip [V.length s..] (V.toList $ forecast periods f s)
    ]
  where
    plotWithRadius radius p = Chart.plot $ do
      p' <- p
      return (p' & plot_points_style . point_radius .~ radius)
#endif
