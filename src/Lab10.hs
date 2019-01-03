--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 10: Type-level programming                                             --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Lab10 where

--------------------------------------------------------------------------------

import GHC.TypeLits

--------------------------------------------------------------------------------

type family Not (b :: Bool) :: Bool where

--------------------------------------------------------------------------------

-- | A singleton type for type-level booleans.
data SBool b where
    STrue  :: SBool b
    SFalse :: SBool b

instance Eq (SBool b) where
    STrue  == STrue  = True
    SFalse == SFalse = True

instance Show (SBool b) where
    show STrue  = "STrue"
    show SFalse = "SFalse"

-- | `inot` @b@ computes the boolean negation of @b@.
inot :: SBool b -> SBool (Not b)
inot = undefined

-- | A kind-polymorphic proxy type.
data Proxy (a :: k) = Proxy

class KnownBool b where
    boolVal :: Proxy b -> Bool

--------------------------------------------------------------------------------

data HList (xs :: [*]) :: * where
    HNil  :: HList as
    HCons :: a -> HList as -> HList as

hhead :: HList (a ': as) -> a
hhead = undefined

--------------------------------------------------------------------------------
