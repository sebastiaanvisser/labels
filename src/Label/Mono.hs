-- | Monomorphic labels don't change the type on modification.

{-# LANGUAGE TypeOperators #-}

module Label.Mono
( Mono

, make
, get
, modify
, set
)
where

import Label.Core (Label)

import qualified Label.Core as Core

{-# INLINE make    #-}
{-# INLINE get     #-}
{-# INLINE modify  #-}
{-# INLINE set     #-}

-- | Partial label.

type Mono l f a = l (f -> f) (a -> a)

-- | Create a monomorphic label from a getter and a modification function that
-- doesn't change the type.

make :: (f -> m o) -> ((o -> n o) -> f -> n f) -> Mono (Label m n) f o
make = Core.make

-- | Getter for a monomorphic label.

get :: Monad m => Mono (Label m n) f o -> f -> m o
get = Core.get

-- | Modifier for a monomorphic label.

modify :: Monad n => Mono (Label m n) f o -> (o -> n o) -> f -> n f
modify = Core.modify

-- | Setter for a monomorphic label.

set :: Monad n => Mono (Label m n) f o -> n o -> f -> n f
set l = modify l . const

