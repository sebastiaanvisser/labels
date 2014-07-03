-- | Simple labels with total getters and total and monomorphic modifications.

{-# LANGUAGE TypeOperators #-}

module Label.Simple
( (:->)
, (:~>)
, make
, get
, modify
, set
)
where

import qualified Label.Partial as Partial
import qualified Label.Total   as Total

{-# INLINE make   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}

-- | Simple monomorphic total label.

type f :-> a = (f -> f) Total.:-> (a -> a)

-- | Simple monomorphic partial label.

type f :~> a = (f -> f) Partial.:~> (a -> a)

-- | Create a total label from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- > set l (get l f) f == f

make :: (f -> o) -> ((o -> o) -> f -> f) -> f :-> o
make = Total.make

-- | Get the getter function from a label.

get :: (f :-> o) -> f -> o
get = Total.get

-- | Get the modifier function from a label.

modify :: (f :-> o) -> (o -> o) -> f -> f
modify = Total.modify

-- | Get the setter function from a label.

set :: (f :-> o) -> o -> f -> f
set = Total.set

