{-# LANGUAGE DeriveFunctor #-}

module Monads where

-- ReaderState Monad

data ReaderState r s a = ReaderState (r -> s -> (s, a)) deriving Functor

run (ReaderState f) = f

ask :: ReaderState r s r
ask = ReaderState (\r s -> (s, r))

local :: (r -> r) -> ReaderState r s a -> ReaderState r s a
local f m = ReaderState (\r s -> run m (f r) s)

get :: ReaderState r s s
get = ReaderState (\r s -> (s, s))

put :: s -> ReaderState r s ()
put s = ReaderState (\r _ -> (s, ()))

instance Applicative (ReaderState r s) where
  pure = return
  fm <*> xm = do f <- fm
                 x <- xm
                 return (f x)

instance Monad (ReaderState r s) where
  return x = ReaderState (\r s -> (s, x))
  m >>= f = ReaderState (\r s -> let (s', x) = run m r s in run (f x) r s')
