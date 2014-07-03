-- | Pointwise labels allow access to multiple points inside a datatype.

{-# LANGUAGE TypeOperators #-}

module Label.Pointwise
( Pointwise
, make
, get
, modify
, set
, folded
, embed
)
where

import Control.Monad.Identity
import Data.Monoid
import Data.Foldable
import Label.Core (Label)

import qualified Label.Core  as Core
import qualified Label.Total as Total

{-# INLINE make   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE folded #-}

-- | Pointwise label.

type Pointwise f o = Label [] Identity f o

-- | Create a pointwise label from a getter that can return multiple values and
-- a some modifier.

make :: (f -> [o]) -> ((o -> i) -> f -> g) -> Pointwise (f -> g) (o -> i)
make g s = Core.make g (\m -> Identity . s (runIdentity . m))

-- | Getter for a pointwise label.

get :: Pointwise (f -> g) (o -> i) -> f -> [o]
get = Core.get

-- | Modifier for a pointwise label.

modify :: Pointwise (f -> g) (o -> i) -> (o -> i) -> f -> g
modify l m = runIdentity . Core.modify l (Identity . m)

-- | Setter for a pointwise label.

set :: Pointwise (f -> g) (o -> i) -> i -> f -> g
set l = modify l . const

-- | Getter for a pointwise label that maps the output value to a monoid and
-- appends them.

folded :: Monoid c => Pointwise (f -> g) (o -> i) -> (o -> c) -> f -> c
folded l f = foldMap f . get l

-- | Embed a total label pointing to a list of items into a pointwise label.

embed :: (f -> g) Total.:-> ([o] -> [i]) -> Pointwise (f -> g) (o -> i)
embed l = make (Total.get l) (\m -> Total.modify l (map m))

