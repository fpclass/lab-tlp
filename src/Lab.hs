--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Type-level programming                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lab where

--------------------------------------------------------------------------------

import GHC.TypeLits

import Data.List (intercalate)
import Data.Kind (Constraint)

--------------------------------------------------------------------------------

type family Not (b :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

--------------------------------------------------------------------------------

-- | A singleton type for type-level booleans.
data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

instance Eq (SBool b) where
    STrue  == STrue  = True
    SFalse == SFalse = True

instance Show (SBool b) where
    show STrue  = "STrue"
    show SFalse = "SFalse"

-- brokenNot :: Bool -> Bool
-- brokenNot True = True
-- brokenNot False = False

-- | `inot` @b@ computes the boolean negation of @b@.
inot :: SBool b -> SBool (Not b)
inot STrue = SFalse
inot SFalse = STrue

type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'True 'True = 'True
    And x y = 'False

iand :: SBool a -> SBool b -> SBool (And a b)
iand STrue STrue = STrue
iand SFalse _ = SFalse
iand _ SFalse = SFalse

-- import Data.Proxy
-- | A kind-polymorphic proxy type.
data Proxy (a :: k) = Proxy

class KnownBool (b :: Bool) where
    boolVal :: Proxy b -> Bool

instance KnownBool 'True where 
    boolVal _ = True

instance KnownBool 'False where 
    boolVal _ = False

bar :: SBool a -> Proxy a
bar _ = Proxy

-- foo :: (KnownBool (Not (And a b))) 
--     => SBool a -> SBool b -> Bool
foo a b = boolVal (bar (inot (iand a b)))

--------------------------------------------------------------------------------

data HList (xs :: [*]) :: * where
    HNil  :: HList '[]
    HCons :: a -> HList as -> HList (a ': as)

hhead :: HList (a ': as) -> a
hhead (HCons x _) = x

-- instance Show (HList '[]) where
--     show HNil = "[]"

-- instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
--     show (HCons x xs) = show x ++ " : " ++ show xs

type family CanShowAll (ts :: [*]) :: Constraint where
    CanShowAll '[] = ()
    CanShowAll (t ': ts) = (Show t, CanShowAll ts)

instance CanShowAll ts => Show (HList ts) where
    show xs = "[" ++ intercalate "," (go xs) ++ "]"
        where go :: CanShowAll as => HList as -> [String]
              go HNil = []
              go (HCons x xs) = show x : go xs

--------------------------------------------------------------------------------
