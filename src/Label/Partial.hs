-- | Partial polymorphic labels for getters that might fail.

{-# LANGUAGE TypeOperators #-}

module Label.Partial
( Partial
, (:~>)
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

-- | Partial label.

type Partial m f a = Label Maybe m f a

-- | Simple partial label.

type f :~> a = Label Maybe Identity f a

-- | Create a partial label from a getter that can fail and a total
-- modification function.

make :: (f -> Maybe o) -> ((o -> m i) -> f -> m g) -> Partial m (f -> g) (o -> i)
make g m = Core.make g m

-- | Getter for a partial label.

get :: (f -> g) :~> (o -> i) -> f -> Maybe o
get = Core.get

-- | Modifier for a partial label.

modify :: (f -> g) :~> (o -> i) -> (o -> i) -> f -> g
modify l m = runIdentity . Core.modify l (Identity . m)

-- | Setter for a partial label.

set :: ((f -> g) :~> (o -> i)) -> i -> f -> g
set l m = runIdentity . Core.set l (Identity m)

