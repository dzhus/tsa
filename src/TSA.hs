module TSA

where

import Control.Applicative

type Series a = [a]

diff :: Num a => Series a -> Series a
diff series = getZipList $ (-) <$> ZipList (drop 1 series) <*> ZipList series
