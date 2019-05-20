{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Plugin.SingletonOptimizer
import Data.Type.Equality ((:~:)(Refl))
import System.Exit (exitSuccess, exitFailure)
import System.Timeout (timeout)
import Control.Exception (Exception, throw, catch, evaluate)
import Data.Functor(($>))
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Data.Functor.Identity (Identity(..))

import qualified Text.PrettyPrint.ANSI.Leijen as PP


data NotOptimizedException = NotOptimizedException deriving Show

instance Exception NotOptimizedException

----------------------------------------------
-- Helpers

-- | Raise an exception if the argument terminates
-- (and obviously if the whole expression does not get optimized away).
-- It has to be applied internally to every top-level binding, for example
-- @unit = ex ()@, so it has a very short name.
{-# ANN ex UnsafeTotal #-}
{-# NOINLINE ex #-}
ex :: a -> a
ex singleton = singleton `seq` throw NotOptimizedException

shouldAlsoTerminate :: IO Bool -> IO Bool
shouldAlsoTerminate f = fromMaybe False <$> timeout 1000000 f

canHang :: IO Bool -> IO Bool
canHang f = fromMaybe True <$> timeout 1000000 f

expectCorrectlyOptimized :: a -> IO Bool
expectCorrectlyOptimized singl =
  shouldAlsoTerminate $
  catch
    (evaluate singl $> True)
    (const $ pure False :: NotOptimizedException -> IO Bool)

expectCorrectlyUnoptimized :: a -> IO Bool
expectCorrectlyUnoptimized singl =
  canHang $
  catch
    (evaluate singl $> False)
    (const $ pure True :: NotOptimizedException -> IO Bool)

reportResults :: String -> IO Bool -> IO Bool
reportResults desc ioRes = do
  res <- ioRes
  let passfail =
        PP.dullblack $
          if res
          then
            PP.ondullgreen $ PP.text "PASS"
          else
            PP.ondullred $ PP.text "FAIL"
      line = passfail <> PP.colon PP.<+> PP.text desc <> PP.line
  PP.putDoc line
  pure res

-- TODO unify with reportResults
knownFailure :: String -> IO Bool -> IO Bool
knownFailure desc ioRes = do
  res <- ioRes
  let passfail =
        PP.dullblack $
          if res
          then
            PP.ondullred $ PP.text "UNEXPECTED PASS"
          else
            PP.ondullyellow $ PP.text "KNOWN FAIL"
      line = passfail <> PP.colon PP.<+> PP.text desc <> PP.line
  PP.putDoc line
  pure $ not res


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
  , testF "Whitelisted module-external references" whitelistedExternalRefs
  , testF "Module-external data constructor" dataCon
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
whitelistedExternalRefs = ex $ trace `seq` (1+1::Integer) `seq` (1+1::Int) `seq` ()

{-# ANN dataCon OptimizeSingleton #-}
dataCon :: Identity ()
dataCon = ex $ Identity ()

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

