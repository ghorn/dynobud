{-# OPTIONS_GHC -Wall #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE DataKinds #-}
--
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts, GADTs, TypeOperators #-}

module TypeNats ( module Types
                , module Types.Data.Num
                , module Types.Data.Num.Decimal
                , module Types.Data.Num.Ops
                , module Types.Data.Ord
                ) where

import Types
--import Types.Base
--import Types.Data.Bool
import Types.Data.Num
import Types.Data.Ord
import Types.Data.Num.Ops
import Types.Data.Num.Decimal
--import Types.Data.List

--x :: D0
--x = modT d0 d0
--
--foo :: n0 -> n1 -> n0 :+: n1
--foo x y = addT x y
--
----apFoo :: D2
----apFoo = addT d0 d1
--
----apFoo :: D1 -> D3 -> D4
--apFoo :: a -> b -> (a :+: b)
--apFoo x y = addT x y
--
--
--blah :: (NaturalT n) => n -> n
--blah x = x
--
--fooo :: D1
--fooo = blah d1

--instance (IntegerT n0, IntegerT n1, (n0 :+: n1) ~ n, IntegerR (Repr n)) => IntegerT n
--instance (NaturalT n0, NaturalT n1, (n0 :+: n1) ~ n) => NaturalT n
