{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Plugin.SingletonOptimizer
import Data.Type.Equality ((:~:)(Refl))
import System.Exit (exitSuccess, exitFailure)
import System.Timeout (timeout)
import Control.Exception (Exception, throw, catch, evaluate)
import Data.Functor(($>))
import Data.Maybe (fromMaybe, isNothing)


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

shouldHang :: IO a -> IO Bool
shouldHang f = isNothing <$> timeout 1000000 f

expectCorrectlyOptimized :: a -> IO Bool
expectCorrectlyOptimized singl =
  shouldAlsoTerminate $
  catch
    (evaluate singl $> True)
    (const $ pure False :: NotOptimizedException -> IO Bool)

expectCorrectlyUnoptimized :: a -> IO Bool
expectCorrectlyUnoptimized singl =
  shouldHang $
  catch
    (evaluate singl $> ())
    (const mempty :: NotOptimizedException -> IO ())

expectIncorrectlyUnoptimized :: a -> IO Bool
expectIncorrectlyUnoptimized singl =
  shouldAlsoTerminate $
  catch
    (evaluate singl $> False)
    (const $ pure True :: NotOptimizedException -> IO Bool)


----------------------------------------------
-- Actual tests

----------------------------------------------
-- Correctly optimized

correctlyOptimized :: [IO Bool]
correctlyOptimized = let e = expectCorrectlyOptimized in
  [ e trivialEq
  , e indirectUnit2, e indirectUnit1, e unit
  , e unsafeFalselyTotal ]

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
unsafeFalselyTotal = unsafeFalselyTotal

----------------------------------------------
-- Correctly unoptimized

correctlyUnoptimized :: [IO Bool]
correctlyUnoptimized =
  [ ]

----------------------------------------------
-- Incorrectly unoptimized

incorrectlyUnoptimized :: [IO Bool]
incorrectlyUnoptimized =
  [ ]

---- Putting it all together

tests :: [IO Bool]
tests = correctlyOptimized
     <> correctlyUnoptimized
     <> incorrectlyUnoptimized

main :: IO ()
main = do
  success <- and <$> sequence tests
  if success then exitSuccess else exitFailure

