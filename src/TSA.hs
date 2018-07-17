{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module TSA

where

import qualified Data.Vector.Generic as V
import qualified Statistics.Sample as S

type Series s e = V.Vector s e

diff :: (Num e, Series s e) => Int -> s e -> s e
diff n series = V.zipWith (-) (V.drop n series) series

type Model s e = Int -> s e -> s e

mean :: Series s Double => Model s Double
mean n s = V.replicate n $ S.mean s

forecast :: Int -> Model s e -> s e -> s e
forecast periods f = f periods
