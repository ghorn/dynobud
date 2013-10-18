{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
--{-# Language UndecidableInstances #-}
--{-# Language DeriveFunctor #-}

module Vectorize ( Vectorize(..)
                 , fill
                 ) where

--import Data.TypeLevel.Num.Ops
import Data.TypeLevel.Num.Sets
import qualified Data.Vector as V

import TypeVecs

class (Functor f, Nat n) => Vectorize f n | f -> n where
  vectorize :: f a -> Vec n a
  devectorize :: Vec n a -> f a
  empty :: f ()

fill :: Vectorize f n => a -> f a
fill x = fmap (const x) empty

instance Nat n => Vectorize (Vec n) n where
  vectorize = id
  devectorize = id
  empty = ret
    where
      ret = unsafeVec (V.replicate k ())
      k = vlength ret

-- test:
--data Pair f g a = Pair (f a) (g a) deriving Functor
--
--instance (Vectorize f1 n1, Vectorize f2 n2, Add n1 n2 n) => Vectorize (Pair f1 f2) n where
--  vectorize (Pair vx vu) = (vectorize vx) <++> (vectorize vu)
--  empty = Pair empty empty
--  devectorize vs = Pair x u
--    where
--      k = asLengthOf x
--      asLengthOf :: (Vectorize f n) => f a -> n
--      asLengthOf _ = undefined
--
--      (x', u') = vsplit k vs
--      x = devectorize x'
--      u = devectorize u'
