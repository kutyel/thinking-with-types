{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}

module Chapter4 where

import           Data.Proxy    (Proxy)
import           Data.Typeable

-- scopedTypeVariables, explicit forall...

works :: forall a b. (a -> b) -> a -> b
works f a = apply
  where
    apply :: b
    apply = f a

-- ambiguous types

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

-- *Chapter4> typeName @Bool
-- "Bool"
-- *Chapter4> typeName @String
-- "[Char]"
-- *Chapter4> typeName @(Maybe [Int])
-- "Maybe [Int]"
