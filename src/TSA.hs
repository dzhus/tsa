{-# LANGUAGE ConstraintKinds #-}

module TSA

where

import qualified Data.Vector.Generic as V

type Series s e = V.Vector s e

diff :: (Num e, Series s e) => Int -> s e -> s e
diff n series = V.zipWith (-) (V.drop n series) series
