-- | Traversing labels can be done directly with any label that has no
-- resitrictions on the modifier context.

module Label.Traversing
( traverse
, write
, lifted
)
where

import Data.Traversable (Traversable, mapM)
import Control.Monad hiding (mapM)
import Label.Core
import Prelude hiding (mapM)

{-# INLINE traverse #-}
{-# INLINE write    #-}

-- | Modifier within some context.

traverse :: Label m n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
traverse = modify

-- | Setter within some context.

write :: Label m n (f -> g) (o -> i) -> n i -> f -> n g
write l v = traverse l (const v)

-- | Lifted label composition.
--
-- For example, when specialized to simple labels and lists:
--
-- > :: (f :-> [o])
-- > -> (o :-> [a])
-- > -> (f :-> [a])

lifted :: (Monad m, Monad n, Monad r, Traversable r)
       => Label m n (f -> g) (r o -> r p)
       -> Label m n (o -> p) (r i -> r j)
       -> Label m n (f -> g) (r i -> r j)
lifted a b = make (liftM join . mapM (get b) <=< get a)
                  (modify a . mapM . modify b)

