{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC.Plugin.SingletonOptimizer
import GHC.Plugin.SingletonOptimizer.Whitelist.Types
import Data.Type.Equality ((:~:)(Refl))
import System.Exit (exitSuccess, exitFailure)
import Control.Exception (throw)
import Debug.Trace (trace)
import Data.Functor.Identity (Identity(..))

import Helpers


----------------------------------------------
-- Helpers

-- | Raise an exception if the argument terminates
-- (and obviously if the whole expression does not get optimized away).
-- It has to be applied internally to every top-level binding, for example
-- @unit = ex ()@, so it has a very short name.
-- This needs to be in the same module as tests so the optimizer can trust it
{-# ANN ex UnsafeTotal #-}
{-# NOINLINE ex #-}
ex :: a -> a
ex singleton = singleton `seq` throw NotOptimizedException

----------------------------------------------
-- Actual tests

----------------------------------------------
-- Expression that should be optimized

shouldOptimizeTests :: [IO Bool]
shouldOptimizeTests =
  let cdesc = "Should optimize - "
      test  desc = reportResults (cdesc <> desc) . expectOptimized
      testF desc = knownFailure  (cdesc <> desc) . expectOptimized
  in
  [ test  "Trivial equality" trivialEq
  , test  "Unit" unit
  , test  "Indirect unit 1" indirectUnit1
  , test  "Indirect unit 2" indirectUnit2
  , test  "UnsafeTotal usage" unsafeFalselyTotal
  , test  "Whitelisted module-external references" whitelistedExternalRefs
  -- This fails because of the non-recursive singleton check.
  , testF "Module-external data constructor" dataCon
  -- This cannot be made to work unless LH starts checking typeclass instances
  , testF "Typeclass method (and DFunId argument)" typeclassMethod
  , test  "Calls a structurally terminating function" callsStructurallyTerminating
  -- This requires enabling the metric-based termination checker in addition
  -- to the structural termination checker
  , testF "Calls a metric-terminating function" callsMetricTerminating
  -- GADT
  , test  "Calls a large term" callsLargeTerm
  ]

{-# ANN trivialEq OptimizeSingleton #-}
{-# NOINLINE trivialEq #-}
trivialEq :: () :~: ()
trivialEq = ex Refl

{-# ANN indirectUnit2 OptimizeSingleton #-}
{-# NOINLINE indirectUnit2 #-}
indirectUnit2 :: ()
indirectUnit2 = ex indirectUnit1

{-# ANN indirectUnit1 OptimizeSingleton #-}
{-# NOINLINE indirectUnit1 #-}
indirectUnit1 :: ()
indirectUnit1 = ex unit

{-# ANN unit OptimizeSingleton #-}
{-# NOINLINE unit #-}
unit :: ()
unit = ex ()

{-# ANN unsafeFalselyTotal OptimizeSingleton #-}
{-# ANN unsafeFalselyTotal UnsafeTotal #-}
{-# NOINLINE unsafeFalselyTotal #-}
unsafeFalselyTotal :: ()
unsafeFalselyTotal = ex unsafeFalselyTotal

{-# ANN whitelistedExternalRefs OptimizeSingleton #-}
whitelistedExternalRefs :: ()
whitelistedExternalRefs = ex $ trace `seq` (1::Integer) `seq` ()

{-# ANN dataCon OptimizeSingleton #-}
dataCon :: Identity ()
dataCon = ex $ Identity ()

class C a where
 cm :: a

instance C () where
 cm = cm

{-# ANN typeclassMethod OptimizeSingleton #-}
typeclassMethod :: ()
typeclassMethod = ex cm

bigN :: N
bigN = S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S$S Z

structurallyTerminating :: N -> ()
structurallyTerminating Z = ()
structurallyTerminating (S n) = structurallyTerminating n

{-# ANN callsStructurallyTerminating OptimizeSingleton #-}
callsStructurallyTerminating :: ()
callsStructurallyTerminating = ex $ structurallyTerminating bigN

metricTerminating :: Int -> ()
metricTerminating n | n <= 0 = ()
metricTerminating n = metricTerminating (n-1)

{-# ANN callsMetricTerminating OptimizeSingleton #-}
callsMetricTerminating :: ()
callsMetricTerminating = ex $ metricTerminating 32

data TagProxy :: Tag -> * where
    TagProxy :: TagProxy 'Tag1

data Tag = Tag1 | Tag2

data Tagged :: Tag -> * where
    T1 :: Tagged 'Tag1
    Trec :: Tagged k -> Tagged k

tag1Proof :: Tagged k -> k :~: 'Tag1
tag1Proof T1 = Refl
tag1Proof (Trec t) = tag1Proof t

onlyTag1 :: Tagged k -> f k -> f 'Tag1
onlyTag1 t x = case tag1Proof t of Refl -> x

largeTerm :: N -> Tagged 'Tag1
largeTerm Z = T1
largeTerm (S n) = Trec $ largeTerm n

{-# ANN callsLargeTerm OptimizeSingleton #-}
{-# NOINLINE callsLargeTerm #-}
callsLargeTerm :: TagProxy 'Tag1
callsLargeTerm = ex $ onlyTag1 (largeTerm bigN :: Tagged 'Tag1) (TagProxy :: TagProxy 'Tag1)

----------------------------------------------
-- Expressions that should not be optimized

shouldNotOptimizeTests :: [IO Bool]
shouldNotOptimizeTests =
  let cdesc = "Should not optimize - "
      test desc = reportResults (cdesc <> desc) . expectUnoptimized
      testF desc = knownFailure  (cdesc <> desc) . expectUnoptimized in
  [ test "Trivial loop" trivialLoop
  , test "Loop in a where clause" loopInWhere
  , test "Module-external reference" externalRef
  , test "Mutually recursive looping bindings" mutuallyRec1
  , test "Infinite term 1" callsInfTerm1
  , test "Infinite term 2" callsInfTerm2
  , testF "Datatype-encoded recursion" callsRecWithData
  ]

{-# ANN trivialLoop OptimizeSingleton #-}
trivialLoop :: ()
trivialLoop = ex trivialLoop

{-# ANN loopInWhere OptimizeSingleton #-}
loopInWhere :: ()
loopInWhere = ex f
  where
    {-# NOINLINE f #-}
    f = f

{-# ANN externalRef OptimizeSingleton #-}
externalRef :: ()
externalRef = ex $ length [()] `seq` ()

{-# ANN mutuallyRec1 OptimizeSingleton #-}
{-# NOINLINE mutuallyRec1 #-}
mutuallyRec1 :: ()
mutuallyRec1 = ex mutuallyRec2

{-# NOINLINE mutuallyRec2 #-}
mutuallyRec2 :: ()
mutuallyRec2 = mutuallyRec1

infT :: Tagged 'Tag1
infT = Trec infT

infN :: N
infN = S infN

{-# ANN callsInfTerm1 OptimizeSingleton #-}
callsInfTerm1 :: TagProxy 'Tag1
callsInfTerm1 = ex $ onlyTag1 (largeTerm infN :: Tagged 'Tag1) (TagProxy :: TagProxy 'Tag1)

{-# ANN callsInfTerm2 OptimizeSingleton #-}
callsInfTerm2 :: TagProxy 'Tag1
callsInfTerm2 = ex $ onlyTag1 (infT :: Tagged 'Tag1) (TagProxy :: TagProxy 'Tag1)

-- Datatype-encoded recursion
newtype Rec a = MkRec (Rec a -> a)

{-# NOINLINE recWithData #-}
recWithData :: Rec a -> a
recWithData r@(MkRec f) = f r

{-# ANN callsRecWithData OptimizeSingleton #-}
callsRecWithData :: (Int :~: Bool)
callsRecWithData = ex $ recWithData (MkRec recWithData)

----------------------------------------------
-- Putting it all together

tests :: [IO Bool]
tests = shouldOptimizeTests
     <> shouldNotOptimizeTests

main :: IO ()
main = do
  success <- and <$> sequence tests
  if success then exitSuccess else exitFailure

