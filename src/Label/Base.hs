-- | Labels for data types in the base package. The label types are kept
-- abstract to be fully reusable in custom contexts. Build to be imported
-- qualified.

{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, TypeOperators #-}

module Label.Base
(
-- * Labels for lists.
  head
, tail
, list
, at

-- * Labels for Either.
, left
, right

-- * Label for Maybe.
, just

-- * Labels for 2-tuples.
, fst
, snd
, swap
, pair

-- * Labels for 3-tuples.
, fst3
, snd3
, trd3
, triple

-- * Read/Show isomorphism.
, readShow
)
where

import Control.Applicative
import Data.Traversable (traverse)
import Label.Core (Iso(..), Label, make)
import Label.Derive (getLabel)
import Label.Mono (Mono)
import Prelude hiding (fst, snd, head, tail)

import qualified Data.Tuple as Tuple

-- | Label pointing to the head of a list's cons cell. (Partial and monomorphic)

head :: (Alternative m, Applicative n)
     => Mono (Label m n) [a] a

-- | Label pointing to the tail of a list's cons cell. (Partial and monomorphic)

tail :: (Alternative m, Applicative n)
     => Mono (Label m n) [a] [a]

(head, tail) = $(getLabel ''[])

-- | Pointwise label for all items in a list.

list :: Applicative m => Label [] m ([o] -> [i]) (o -> i)
list = make id traverse

-- | Partial label for indexed access into a list.

at :: (Alternative m, Applicative n) => Int -> Label m n ([a] -> [a]) (a -> a)
at i = make (\ls   -> if length ls > i
                      then pure (ls !! i)
                      else empty)
            (\f ls -> if length ls > i
                      then (take i ls ++) <$> ((: drop (i + 1) ls) <$> f (ls !! i))
                      else pure ls
             )

-- | Label pointing to the left value in an Either. (Partial and polymorphic)

left :: (Applicative n, Alternative m)
     => Label m n (Either a b -> Either c b) (a -> c)

-- | Label pointing to the right value in an Either. (Partial and polymorphic)

right :: (Applicative n, Alternative m)
      => Label m n (Either a b -> Either a c) (b -> c)

(left, right) = $(getLabel ''Either)

-- | Label pointing to the value in a Maybe. (Partial and polymorphic)

just :: (Applicative n, Alternative m)
     => Label m n (Maybe a -> Maybe b) (a -> b)

just = $(getLabel ''Maybe)

-- | Label pointing to the first component of a 2-tuple. (Total and polymorphic)

fst :: (Applicative n, Applicative m)
    => Label m n ((a, b) -> (c, b)) (a -> c)

-- | Label pointing to the second component of a 2-tuple. (Total and polymorphic)

snd :: (Applicative n, Applicative m)
    => Label m n ((a, b) -> (a, c)) (b -> c)

(fst, snd) = $(getLabel ''(,))

-- | Polymorphic label that swaps the components of a tuple. (Total and polymorphic)

swap :: Applicative m => Iso m m (a, b) (b, a)
swap = Iso (pure . Tuple.swap) (pure . Tuple.swap)

-- | Pointwise access to the two items in a pair.

pair :: Applicative m
     => Label [] m ((o, o) -> (a, a)) (o -> a)
pair = make (\(a, b) -> [a, b]) (\m (a, b) -> (,) <$> m a <*> m b)

-- | Label pointing to the first component of a 3-tuple. (Total and polymorphic)

fst3 :: (Applicative m, Applicative n)
     => Label m n ((a, b, c) -> (d, b, c)) (a -> d)

-- | Label pointing to the second component of a 3-tuple. (Total and polymorphic)

snd3 :: (Applicative m, Applicative n)
     => Label m n ((a, b, c) -> (a, d, c)) (b -> d)

-- | Label pointing to the third component of a 3-tuple. (Total and polymorphic)

trd3 :: (Applicative m, Applicative n)
     => Label m n ((a, b, c) -> (a, b, d)) (c -> d)

(fst3, snd3, trd3) = $(getLabel ''(,,))

-- | Pointwise access to the three items in a triple.

triple :: Applicative m
       => Label [] m ((o, o, o) -> (a, a, a)) (o -> a)
triple = make (\(a, b, c) -> [a, b, c]) (\m (a, b, c) -> (,,) <$> m a <*> m b <*> m c)

-- | Partial isomorphism for readable and showable values. Can easily be lifted
-- into a label by using `iso`.

readShow :: (Alternative m, Applicative n, Read a, Show a)
         => Iso m n String a
readShow = Iso r s
  where r v = case readsPrec 0 v of
                (w, _):_ -> pure w
                []       -> empty
        s = pure . show

