{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}

module Chapter15 where

-- Dependent Types 🤯

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Typeable

data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

fromSBool :: SBool b -> Bool
fromSBool STrue = True
fromSBool SFalse = False

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

withSomeSBool
  :: SomeSBool
  -> (forall (b :: Bool). SBool b -> r)
  -> r
withSomeSBool (SomeSBool s) f = f s

toSBool :: Bool -> SomeSBool
toSBool True = SomeSBool STrue
toSBool False = SomeSBool SFalse

-- ad-hoc implementation

class Monad (LogginMonad b)
    => MonadLogging (b :: Bool) where
  type LogginMonad b = (r :: Type -> Type) | r -> b
  logMsg :: String -> LogginMonad b ()
  runLogging :: LogginMonad b a -> IO a

instance MonadLogging 'False where
  type LogginMonad 'False = IO
  logMsg _ = pure ()
  runLogging = id

instance MonadLogging 'True where
  type LogginMonad 'True = WriterT [String] IO
  logMsg s = tell [s]
  runLogging m = do
    (a, w) <- runWriterT m
    for_ w putStrLn
    pure a

program :: MonadLogging b => LogginMonad b ()
program = do
  logMsg "hello world"
  pure ()

main :: IO ()
main = do
  bool <- read <$> getLine
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program

dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue = Dict
dict SFalse = Dict

-- generalized machinery
data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing
  :: SomeSing k
  -> (forall (a :: k). Sing a -> r)
  -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
  type Demote k = r | r -> k
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k

data instance Sing (a :: Bool) where
  SSTrue :: Sing 'True
  SSFalse :: Sing 'False

instance SingKind Bool where
  type Demote Bool = Bool
  toSing True = SomeSing SSTrue
  toSing False = SomeSing SSFalse
  fromSing SSTrue = True
  fromSing SSFalse = False

class SingI (a :: k) where
  sing :: Sing a

instance SingI 'True where
  sing = SSTrue

instance SingI 'False where
  sing = SSFalse

data instance Sing (a :: Maybe k) where
  SJust :: Sing (a :: k) -> Sing ('Just a)
  SNothing :: Sing 'Nothing

instance SingI a => SingI ('Just a) where
  sing = SJust sing

instance SingI 'Nothing where
  sing = SNothing

instance (k ~ Demote k, SingKind k)
    => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k
  toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
  toSing Nothing = SomeSing SNothing
  fromSing (SJust a) = Just $ fromSing a
  fromSing SNothing = Nothing

data instance Sing (a :: [k]) where
  SNil :: Sing '[]
  SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)

instance (k ~ Demote k, SingKind k)
    => SingKind [k] where
  type Demote [k] = [k]
  toSing [] = SomeSing SNil
  toSing (h : t) = withSomeSing (toSing h) $ \sh ->
    withSomeSing (toSing t) $ \st -> SomeSing $ SCons sh st
  fromSing SNil = []
  fromSing (SCons sh st) = fromSing sh : fromSing st

-- exercise 15.3-i
instance SingI '[] where
  sing = SNil

instance (SingI h, SingI t) => SingI (h ': t) where
  sing = SCons sing sing
