module Monads where

import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S

-- ReaderState Monad
type ReaderState r s = R.ReaderT r (S.State s)

ask :: ReaderState r s r
ask = R.ask

local :: (r -> r) -> ReaderState r s a -> ReaderState r s a
local = R.local

get :: ReaderState r s s
get = lift S.get

put :: s -> ReaderState r s ()
put s = lift (S.put s)

run :: ReaderState r s a -> r -> s -> s
run m r = S.execState (R.runReaderT m r)
