{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter7 where

-- TODO: run ormolu in this file

import Data.Foldable (asum)
import Data.IORef
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Typeable
import System.IO.Unsafe (unsafePerformIO)

data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any x) = f x

-- exercise 7.1-i
-- They can only return constant values.

data HasShow where
  HasShow :: Show t => t -> HasShow

-- instance Show HasShow where
--   show (HasShow s) = "HasShow " ++ show s

-- exercise 7.1-ii
-- It will complain that it cannot be sure that `s` has an instance of Show

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow x) = f x

-- exercise 7.1-iii
instance Show HasShow where
  show = elimHasShow show

-- turning Haskell into Python...

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic x) = f x

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r) => Dynamic -> Dynamic -> (a -> b -> r) -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    , liftD2 @String @Int    a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int    @String a b $ \intA strB -> show intA ++ strB
    ]

-- moar polymorphism!

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has x) = f x

-- type HasShow = Has Show
-- type Dynamic = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

type MonoidAndEq a = (Monoid a, Eq a)

-- constraint synonym

class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

shouldBeFalse :: Bool
shouldBeFalse =
  let foo = Has [True] :: Has MonoidEq
   in elimHas isMempty foo

-- scoping information with existentials

newtype ST s a = ST
  { unsafeRunST :: a
  }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  return = pure
  ST a >>= f = seq a $ f a

-- mutable variables inside the ST "trick"

newtype STRef s a = STRef
  { unSTRef :: IORef a
  }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST :: (forall s. ST s a) -> a
runST = unsafeRunST

-- example of local mutation to return a pure value

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

-- runST safeExample // > "hello world"
