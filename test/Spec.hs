--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 10: Type-level programming                                             --
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE TypeInType #-}

import GHC.TypeLits

import Control.DeepSeq
import Control.Exception

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import qualified Lab10 as L

--------------------------------------------------------------------------------

instance NFData (L.HList '[]) where
    rnf L.HNil = ()

instance NFData x => NFData (L.HList (x ': xs)) where
    rnf (L.HCons x xs) = ()

instance NFData (L.SBool b) where
    rnf _ = ()

--------------------------------------------------------------------------------

data (a :: k1) :~: (b :: k2) where
    Refl :: a :~: a

instance NFData (a :~: b) where
    rnf Refl = ()

-- `shouldTypecheck` @term@ is an `Assertion` which will succeed if @term@
-- successfully typechecks, i.e. does not throw a runtime type error (due to
-- deferred type errors).
shouldTypecheck :: NFData a => a -> Assertion
shouldTypecheck a = do
    result <- try (evaluate (force a))
    case result of
        Right _              -> return ()
        Left (TypeError msg) -> assertFailure "This does not typecheck."

--------------------------------------------------------------------------------

notTrue :: L.Not True :~: False
notTrue = Refl

notFalse :: L.Not False :~: True
notFalse = Refl

sTrue :: L.SBool True
sTrue = L.STrue

sFalse :: L.SBool False
sFalse = L.SFalse

hheadNonEmpty :: Int
hheadNonEmpty = L.hhead (L.HCons 1 L.HNil)

inotTrue :: L.SBool False
inotTrue = L.inot (L.STrue :: L.SBool True)

inotFalse :: L.SBool True
inotFalse = L.inot (L.SFalse :: L.SBool False)

-- | `main` is the main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "Not" $ do
        it "Not True ~ False" $
            shouldTypecheck notTrue
        it "Not False ~ True" $
            shouldTypecheck notFalse
    describe "SBool" $ do
        it "STrue :: SBool True" $
            shouldTypecheck sTrue
        it "SFalse :: SBool False" $
            shouldTypecheck sFalse
        it "SBool False is not a valid type for STrue" $
            shouldNotTypecheck (L.STrue :: L.SBool False)
        it "SBool True is not a valid type for SFalse" $
            shouldNotTypecheck (L.SFalse :: L.SBool True)
        it "inot STrue ==> SFalse" $
            L.inot L.STrue `shouldBe` L.SFalse
        it "inot SFalse ==> STrue" $
            L.inot L.SFalse `shouldBe` L.STrue
        it "inot STrue :: SBool False" $
            shouldTypecheck inotTrue
        it "inot SFalse :: SBool True" $
            shouldTypecheck inotFalse
        it "SBool True is not a valid type for inot STrue" $
            shouldNotTypecheck (L.inot L.STrue :: L.SBool True)
        it "SBool False is not a valid type for inot SFalse" $
            shouldNotTypecheck (L.inot L.SFalse :: L.SBool False)
        it "boolVal (Proxy :: Proxy True) ==> True" $
            L.boolVal (L.Proxy :: L.Proxy True) `shouldBe` True
        it "boolVal (Proxy :: Proxy False) ==> False" $
            L.boolVal (L.Proxy :: L.Proxy False) `shouldBe` False
    describe "HList" $ do
        it "HNil :: HList '[]" $
            shouldTypecheck (L.HNil :: L.HList '[])
        it "HList '[Bool] is not a valid type for HNil" $
            shouldNotTypecheck (L.HNil :: L.HList '[Bool])
        it "HCons True HNil :: HList '[Bool]" $
            shouldTypecheck (L.HCons True L.HNil :: L.HList '[Bool])
        it "HList '[Int] is not a valid type for HCons True HNil" $
            shouldNotTypecheck (L.HCons True L.HNil :: L.HList '[Int])
        it "HCons 4 (HCons True HNil) :: Num a => HList '[a, Bool]" $
            shouldTypecheck (L.HCons 4 (L.HCons True L.HNil) :: L.HList '[Int, Bool])
        it "HList '[Char] is not a valid type for HCons 4 (HCons True HNil)" $
            shouldNotTypecheck (L.HCons 4 (L.HCons True L.HNil) :: L.HList '[Char])
        it "should not allow hhead to be called on an empty list" $
            shouldNotTypecheck (L.hhead L.HNil :: Int)
        it "should be allowed to call hhead on a non-empty list" $
            shouldTypecheck hheadNonEmpty
        it "show HNil ==> \"[]\"" $
            show L.HNil `shouldBe` "[]"
        it "show (HCons 4 HNil) ==> \"4 : []\"" $
            show (L.HCons 4 L.HNil) `shouldBe` "4 : []"
        it "show (HCons \"cake\" (HCons 4 HNil)) ==> \"\\\"cake\\\" : 4 : []\"" $
            show (L.HCons "cake" (L.HCons 4 L.HNil)) `shouldBe` "\"cake\" : 4 : []"

--------------------------------------------------------------------------------
