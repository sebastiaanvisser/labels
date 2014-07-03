-- | State and Reader operations specialized for working with total labels.

{-# LANGUAGE TypeOperators  #-}

module Label.Monadic
(
-- * 'MonadState' label operations.
  gets
, puts
, modify
, modifyAndGet
, (=:)
, (=.)

-- * 'MonadReader' label operations.
, asks
, local
)
where

import Control.Monad
import Label.Simple ((:->))

import qualified Label.Total          as Total
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State

-- | Get a value out of the state, pointed to by the specified label.

gets :: State.MonadState f m => (f :-> o) -> m o
gets = State.gets . Total.get

-- | Set a value somewhere in the state, pointed to by the specified label.

puts :: State.MonadState f m => (f :-> o) -> o -> m ()
puts l = State.modify . Total.set l

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified label.

modify :: State.MonadState f m => (f :-> o) -> (o -> o) -> m ()
modify l = State.modify . Total.modify l

-- | Alias for `puts' that reads like an assignment.

infixr 2 =:
(=:) :: State.MonadState f m => (f :-> o) -> o -> m ()
(=:) = puts

-- | Alias for `modify' that reads more or less like an assignment.

infixr 2 =.
(=.) :: State.MonadState f m => (f :-> o) -> (o -> o) -> m ()
(=.) = modify

-- | Fetch a value pointed to by a label out of a reader environment.

asks :: Reader.MonadReader f m => (f :-> o) -> m o
asks = Reader.asks . Total.get

-- | Execute a computation in a modified environment. The label is used to
-- point out the part to modify.

local :: Reader.MonadReader f m => (f :-> o) -> (o -> o) -> m a -> m a
local l f = Reader.local (Total.modify l f)

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified label. Additionally return a separate value based on the
-- modification.

modifyAndGet :: State.MonadState f m => (f :-> o) -> (o -> (a, o)) -> m a
modifyAndGet l f =
  do (b, a) <- f `liftM` gets l
     puts l a
     return b

