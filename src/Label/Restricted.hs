-- | Restricted polymorphic labels with getters and modifiers that can both fail.

{-# LANGUAGE TypeOperators #-}

module Label.Restricted
( (:~>)
, make
, get
, modify
, set
)
where

import Label.Core (Label)

import qualified Label.Core as Core

{-# INLINE make   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}

-- | Restricted partial label type.

type f :~> a = Label Maybe Maybe f a

-- | Create a restricted label from a getter that can fail and a modification
-- function that can fail.

make :: (f -> Maybe o) -> ((o -> Maybe i) -> f -> Maybe g) -> (f -> g) :~> (o -> i)
make = Core.make

-- | Getter for a restricted label.

get :: (f -> g) :~> (o -> i) -> f -> Maybe o
get = Core.get

-- | Modifier for a restricted label.

modify :: (f -> g) :~> (o -> i) -> (o -> i) -> f -> Maybe g
modify l m = Core.modify l (Just . m)

-- | Setter for a restricted label.

set :: ((f -> g) :~> (o -> i)) -> i -> f -> Maybe g
set l = modify l . const

