{-# OPTIONS_GHC -Wall #-}

module Dyno.GoldenSectionSearch
       ( Golden(..)
       , goldenSectionSearch
       , goldenSectionSearch'
       ) where

tau :: Floating a => a
tau = 2 / (1 + sqrt 5)

data Golden a =
  Golden
  { goldenX :: a
  , goldenY :: a
  , goldenBox :: (a, a)
  } deriving (Show, Eq, Ord)

-- | Iterate a golden section search until the
-- bounding box is withing a given tolerance.
goldenSectionSearch :: (Ord a, Floating a) => a -> (a -> a) -> (a, a) -> (a, a)
goldenSectionSearch eps f bnds = g $ goldenSectionSearch' f bnds
  where
    g ((Golden x fx (lbx, ubx)):gs)
      | ubx - lbx < eps = (x, fx)
      | otherwise = g gs
    g _ = error "goldenSectionSearch': hit the end of an infinite list"

-- | Return an infinite list of the iterations of a golden section search.
goldenSectionSearch' :: (Ord a, Floating a) => (a -> a) -> (a, a) -> [Golden a]
goldenSectionSearch' f (y0, y3) = gss (y0, y1, y2, y3)
  where
    y1 = y0 + (y3 - y0) * (1 - tau)
    y2 = y0 + (y3 - y0) * tau

    gss (x0, x1, x2, x3)
      | f x1 < f x2 = Golden x1 (f x1) (x0, x2) : gss (x0, x1', x1,  x2)
      | otherwise   = Golden x2 (f x2) (x1, x3) : gss (x1, x2,  x2', x3)
      where
        x1' = x0 + (x2 - x0) * (1 - tau)
        x2' = x1 + (x3 - x1) * tau
