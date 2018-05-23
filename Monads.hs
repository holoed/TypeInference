{-# LANGUAGE DeriveFunctor #-}

module Monads where

-- ReaderState Monad

data ReaderState r s a = ReaderState (r -> s -> (s, a)) deriving Functor

apply (ReaderState f) = f

ask :: ReaderState r s r
ask = ReaderState (\r s -> (s, r))

local :: (r -> r) -> ReaderState r s a -> ReaderState r s a
local f m = ReaderState (\r s -> apply m (f r) s)

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
  m >>= f = ReaderState (\r s -> let (s', x) = apply m r s in apply (f x) r s')
