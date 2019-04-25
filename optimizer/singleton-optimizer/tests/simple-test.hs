{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Plugin.SingletonOptimizer ( OptimizeSingleton(OptimizeSingleton) )
import Data.Type.Equality ((:~:)(Refl))
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)
import Control.Monad (unless)
import System.Exit (exitFailure)


-- If the ref gets modified, it means that the unsafePerformIO is still there
-- and the optimization did not trigger

data OptimizationTriggered = Triggered | NotTriggered deriving (Eq, Show)

{-# NOINLINE uref #-}
uref :: IORef OptimizationTriggered
uref = unsafePerformIO $ newIORef Triggered

{-# NOINLINE oref #-}
oref :: IORef OptimizationTriggered
oref = unsafePerformIO $ newIORef Triggered

unoptimizedEquality :: () :~: ()
unoptimizedEquality = seq (unsafePerformIO $ atomicWriteIORef uref NotTriggered) $ trace "Equality not marked as optimized left unchanged" Refl

{-# ANN optimizedEquality OptimizeSingleton #-}
optimizedEquality :: () :~: ()
optimizedEquality = seq (unsafePerformIO $ atomicWriteIORef oref NotTriggered) $ trace "Equality marked as optimized left unchanged" Refl

main = do
  putStrLn "Refs before:"
  or1 <- readIORef oref
  putStrLn $ "or: " <> show or1
  ur1 <- readIORef uref
  putStrLn $ "ur: " <> show ur1
  putStrLn $ unoptimizedEquality `seq` optimizedEquality `seq` "Now forcing the equalities..."
  putStrLn "Refs after:"
  or2 <- readIORef oref
  putStrLn $ "or: " <> show or2
  ur2 <- readIORef uref
  putStrLn $ "ur: " <> show ur2
  case (ur2, or2) of
    (NotTriggered, Triggered) -> putStrLn "The optimization triggered correctly"
    _ -> do
      putStrLn "The optimization did NOT trigger correctly"
      exitFailure

