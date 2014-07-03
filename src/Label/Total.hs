-- | Total labels for for getters that modifiers that cannot fail.

{-# LANGUAGE TypeOperators #-}

module Label.Total
( (:->)
, make
, get
, modify
, set
)
where

import Control.Monad.Identity
import Label.Core (Label)

import qualified Label.Core as Core

{-# INLINE make   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}

-- | Total label type.

type f :-> a = Label Identity Identity f a

-- | Create a total label from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- > set l (get l f) f == f

make :: (f -> o) -> ((o -> i) -> f -> g) -> (f -> g) :-> (o -> i)
make g s = Core.make (Identity . g) (\m -> Identity . s (runIdentity . m))

-- | Getter for a total label.

get :: (f -> g) :-> (o -> i) -> f -> o
get l = runIdentity . Core.get l

-- | Modifier for a total label.

modify :: (f -> g) :-> (o -> i) -> (o -> i) -> f -> g
modify l m = runIdentity . Core.modify l (Identity . m)

-- | Setter for a total label.

set :: ((f -> g) :-> (o -> i)) -> i -> f -> g
set l = modify l . const

