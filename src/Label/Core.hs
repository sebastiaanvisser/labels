-- | The core label data type which generalizes the different labels.

{-# LANGUAGE GADTs, FlexibleInstances #-}

module Label.Core
(
-- * The data accessor label type.
  Label

-- * Operations on labels.
, make
, get
, modify
, set

, view
, (>-)

-- * Working with isomorphisms.
, Iso (..)
, inv
, iso
)
where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad hiding (mapM)

{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE inv    #-}

-- | Abstract Label datatype. The getter and modifier operations work in some
-- monad. The type of the value pointed to might change, thereby changing the
-- type of the outer structure.

data Label m n f o where
  Ops :: !(f -> m o)
      -> !((o -> n i) -> f -> n g)
      -> Label m n (f -> g) (o -> i)
  Id :: Label m n f f

-- | Category based composition of labels.

instance Monad m => Category (Label m n) where
  id                = Id
  Ops a b . Ops c d = Ops (a <=< c) (d . b)
  Id      . u       = u
  u       . Id      = u
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Create a label from an abstract getter and modifier function.

make :: (f -> m o) -> ((o -> n i) -> f -> n g) -> Label m n (f -> g) (o -> i)
make = Ops

-- | Fetch the getter from a label.

get :: Monad m => Label m n (f -> g) (o -> i) -> f -> m o
get (Ops g _) = g
get Id        = return

-- | Fetch the modifier from a label.

modify :: Label m n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
modify (Ops _ m) = m
modify Id        = id

-- | Fetch the setter from a label.

set :: Label m n (f -> g) (o -> i) -> n i -> f -> n g
set l = modify l . const

-------------------------------------------------------------------------------
-- Applicative composition.

infix 7 >-

-- | Use a label to temporarily diverge another label, to be used for
-- Applicative composition.

(>-) :: Monad m
     => Label m m (j -> a) (i -> b)
     -> Label m m (f -> g) (o -> i)
     -> View m f g j o
(>-) (Ops f _) (Ops g m) = View (Ops g (\n -> m (f <=< n)))
(>-) (Ops f _) Id        = View (Ops return (f <=<))
(>-) Id        l         = View l

-- | A view restricts the contexts and allow for changing the output type.

newtype View m f g i o = View (Label m m (f -> g) (o -> i))

-- | Get the label out of a view.

view :: View m f g i o -> Label m m (f -> g) (o -> i)
view (View v) = v

instance Monad m => Functor (View m f f i) where
  fmap f x = pure f <*> x
  {-# INLINE fmap #-}

instance Monad m => Applicative (View m f f i) where
  pure a = View $ Ops (const (return a)) (const return)
  View a <*> View b = View $ Ops
    (liftM2 ap (get a) (get b))
    (\m f -> modify b (\y -> get a f >>= m . ($ y))
         =<< modify a (\x -> get b f >>= m . (x $)) f
    )
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance MonadPlus m => Alternative (View m f f i) where
  empty             = View $ Ops (const mzero) (const (const mzero))
  View a <|> View b = View $ Ops (liftM2 mplus (get a) (get b))
                                 (\m -> liftM2 mplus (modify a m) (modify b m))

-------------------------------------------------------------------------------
-- | Effectful isomorphisms.

infix 8 `Iso`

-- | An isomorphism is like a `Category` that works in two directions.

data Iso m n i o = Iso { fw :: i -> m o, bw :: o -> n i }

-- | Isomorphisms are categories.

instance (Monad m, Monad n) => Category (Iso m n) where
  id = Iso return return
  Iso a b . Iso c d = Iso (a <=< c) (d <=< b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Flip an isomorphism.

inv :: Iso m n i o -> Iso n m o i
inv i = Iso (bw i) (fw i)

-- | Turn a isomorphism into a label.

iso :: Monad m => Iso m m f o -> Iso m m g i -> Label m m (f -> g) (o -> i)
iso (Iso f _) (Iso _ b) = make f (\m -> b <=< m <=< f)

