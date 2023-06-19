{-# LANGUAGE TupleSections #-}

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State s) = State $ \input ->
    let (val, st) = s input
     in (f val, st)

instance Applicative (State s) where
  pure :: a -> State s a
  pure val = State (val,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sg) <*> (State s) = State $ \input ->
    let (g, st) = sg input
        (val, st') = s st
     in (g val, st')

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State s) >>= f = State $ \inp ->
    let (val, st') = s inp
        (State g) = f val
     in g st'

put :: a -> State a ()
put val = State $ const ((), val)

get :: State a a
get = State $ \st -> (st, st)
