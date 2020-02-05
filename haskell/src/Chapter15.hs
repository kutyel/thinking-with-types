{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}

module Chapter15 where

-- Dependent Types 🤯

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)

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
