{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Chapter9 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)

infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type

  format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String

  format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a

  format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a

  format s _ param = format (s <> show param) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a

  format s _ param = format (s <> param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

-- try it out! Look Ma! Just like C++!

-- * Chapter9> printf (Proxy @(String :<< " world!")) "hello"
-- "hello world!"
