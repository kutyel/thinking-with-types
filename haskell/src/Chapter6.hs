{-# LANGUAGE RankNTypes #-}

module Chapter6 where

-- "the rank of a function is simply the number of arrows
-- its deepest forall is to the left of."

-- exercise 6.3-i

-- * Chapter6> :kind Int -> forall a. a -> a
-- Int -> forall a. a -> a :: *
-- (Int -> (forall a. (a -> a)))
-- rank-1

-- exercise 6.3-ii
-- (a -> b) -> (forall c. (c -> a)) -> b
-- rank-2

-- exercise 6.3-iii
-- ((forall x. (m x -> b (z m x))) -> b (z m a)) -> m a
-- rank-3

-- Continuation Monad
cont :: a -> (forall r. (a -> r) -> r)
cont a callback = callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = let callback = id in f callback

newtype Cont a
  = Cont
      { unCont :: forall r. (a -> r) -> r
      }

-- exercise 6.4-i
instance Functor Cont where
  fmap f (Cont c) = Cont (c . (. f))

-- exercise 6.4-ii
instance Applicative Cont where
  pure a = Cont ($ a)

  Cont f <*> Cont a = Cont (f . (a .) . (.))

-- exercise 6.4-ii
instance Monad Cont where
  return = pure

  Cont m >>= f = Cont (m . flip (unCont . f))

-- example

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "macOS"

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date

-- exercise 6.4-iv
-- write the Monad Transformer 😱
newtype ContT m a
  = ContT
      { unContT :: forall r. (a -> m r) -> m r
      }

instance Monad m => Functor (ContT m) where
  fmap f (ContT c) = ContT $ \c' -> c (c' . f)

instance Monad m => Applicative (ContT m) where
  pure a = ContT $ \c -> c a

  ContT f <*> ContT a = ContT $ \br -> f $ \ab -> a (br . ab)

instance Monad m => Monad (ContT m) where
  return = pure

  ContT m >>= f = ContT $ \c -> m $ \a -> unContT (f a) c
