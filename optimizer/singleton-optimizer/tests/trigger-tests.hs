{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Plugin.SingletonOptimizer
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
  , testF "Module-external data constructor" dataCon
  -- This cannot be made to work unless LH starts checking typeclass instances
  , testF "Typeclass method (and DFunId argument)" typeclassMethod
  , test  "Calls a structurally terminating function" callsStructurallyTerminating
  -- This requires enabling the metric-based termination checker in addition
  -- to the structural termination checker
  , testF "Calls a metric-terminating function" callsMetricTerminating
  ]

{-# ANN trivialEq OptimizeSingleton #-}
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

{-# ANN typeclassMethod OptimizeSingleton #-}
typeclassMethod :: ()
typeclassMethod = ex mempty

data N = Z | S N

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

----------------------------------------------
-- Expressions that should not be optimized

shouldNotOptimizeTests :: [IO Bool]
shouldNotOptimizeTests =
  let cdesc = "Should not optimize - "
      test desc = reportResults (cdesc <> desc) . expectUnoptimized in
  [ test "Trivial loop" trivialLoop
  , test "Module-external reference" externalRef
  ]

{-# ANN trivialLoop OptimizeSingleton #-}
trivialLoop :: ()
trivialLoop = ex trivialLoop

{-# ANN externalRef OptimizeSingleton #-}
externalRef :: ()
externalRef = ex $ length [()] `seq` ()

----------------------------------------------
-- Putting it all together

tests :: [IO Bool]
tests = shouldOptimizeTests
     <> shouldNotOptimizeTests

main :: IO ()
main = do
  success <- and <$> sequence tests
  if success then exitSuccess else exitFailure

