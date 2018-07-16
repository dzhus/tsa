{-# LANGUAGE ConstraintKinds #-}

module TSA

where

import qualified Data.Vector.Generic as V

type Series s e = V.Vector s e

diff :: (Num e, Series s e) => s e -> s e
diff series = V.zipWith (-) (V.drop 1 series) series
