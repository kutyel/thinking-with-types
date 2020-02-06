{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Chapter15Singletons where

import Data.Aeson
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Singletons.Prelude
import Data.Singletons.TH
import Unsafe.Coerce (unsafeCoerce)

singletons
  [d|
    data TimeOfDay
      = Morning
      | Afternoon
      | Evening
      | Night
      deriving (Eq, Ord, Show)
    |]

-- data Decision a
--   = Proved a
--   | Disproved (a -> Void)

-- instance (Eq (Demote k), SingKind k)
--     => SDecide k where
--   a %~ b =
--     if fromSing a == fromSing b
--       then Proved $ unsafeCoerce Refl
--       else Disproved $ const undefined

-- instance SDecide Bool where
--   STrue %~ STrue = Proved Refl
--   SFalse %~ SFalse = Proved Refl
--   _ %~ _ = Disproved $ const undefined

-- exercise 15.4-i
-- instance SDecide a => SDecide (Maybe a) where
--   SNothing %~ SNothing = Proved Refl
--   SJust a %~ SJust b =
--     case a %~ b of
--       Proved Refl -> Proved Refl
--       Disproved _ -> Disproved $ const undefined

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma ::
  (forall (a :: k). Sing a -> f a -> r) ->
  Sigma f ->
  r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma = Sigma sing

fromSigma ::
  forall k (a :: k) (f :: k -> Type).
  (SingI a, SDecide k) =>
  Sigma f ->
  Maybe (f a)
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl -> Just f
    Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

instance
  (Dict1 Eq (f :: k -> Type), SDecide k) =>
  Eq (Sigma f)
  where
  Sigma sa fa == Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

instance
  ( Dict1 Show (f :: k -> Type),
    Show (Demote k),
    SingKind k
  ) =>
  Show (Sigma f)
  where
  show (Sigma sa fa) =
    case dict1 @Show @f sa of
      Dict ->
        mconcat
          [ "Sigma ",
            show $ fromSing sa,
            " (",
            show fa,
            ")"
          ]

-- exercise 15.5-i
instance
  ( Dict1 Eq (f :: k -> Type),
    Dict1 Ord f,
    SDecide k,
    SingKind k,
    Ord (Demote k)
  ) =>
  Ord (Sigma f)
  where
  Sigma sa fa `compare` Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Ord @f sa of
          Dict -> fa `compare` fb
      Disproved _ ->
        fromSing sa `compare` fromSing sb

-- structured logging

singletons
  [d|
    data LogType
      = JsonMsg
      | TextMsg
      deriving (Eq, Ord, Show)
    |]

data family LogMsg (msg :: LogType)

data instance LogMsg 'JsonMsg = Json Value
  deriving (Eq, Show)

data instance LogMsg 'TextMsg = Text String
  deriving (Eq, Show)

instance
  (c (LogMsg 'JsonMsg), c (LogMsg 'TextMsg)) =>
  Dict1 c LogMsg
  where
  dict1 SJsonMsg = Dict
  dict1 STextMsg = Dict

logs :: [Sigma LogMsg]
logs =
  [ toSigma $ Text "hello",
    toSigma $ Json $
      object ["world" .= (5 :: Int)],
    toSigma $ Text "structured logging is cool"
  ]

showLogs :: [Sigma LogMsg] -> [String]
showLogs = fmap $ withSigma $ \sa fa ->
  case dict1 @Show @LogMsg sa of
    Dict -> show fa

catSigmas ::
  forall k (a :: k) f.
  (SingI a, SDecide k) =>
  [Sigma f] ->
  [f a]
catSigmas = mapMaybe fromSigma

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs
