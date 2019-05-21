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
-- Correctly optimized

correctlyOptimized :: [IO Bool]
correctlyOptimized =
  let cdesc = "Should optimize - "
      test  desc = reportResults (cdesc <> desc) . expectCorrectlyOptimized
      testF desc = knownFailure  (cdesc <> desc) . expectCorrectlyOptimized
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

----------------------------------------------
-- Correctly unoptimized

correctlyUnoptimized :: [IO Bool]
correctlyUnoptimized =
  let cdesc = "Should not optimize - "
      test desc = reportResults (cdesc <> desc) . expectCorrectlyUnoptimized in
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
tests = correctlyOptimized
     <> correctlyUnoptimized

main :: IO ()
main = do
  success <- and <$> sequence tests
  if success then exitSuccess else exitFailure

