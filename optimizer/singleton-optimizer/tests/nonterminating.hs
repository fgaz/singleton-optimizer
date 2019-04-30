{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Plugin.SingletonOptimizer ( OptimizeSingleton(OptimizeSingleton) )
import Data.Type.Equality ((:~:))
import System.Exit (exitSuccess, exitFailure)
import System.Timeout (timeout)


{-# ANN nonterminatingEquality OptimizeSingleton #-}
nonterminatingEquality :: () :~: ()
-- Hopefully the GHC rts does not detect the <<<loop>>>
nonterminatingEquality = nonterminatingEquality

main :: IO ()
main = do
  result <- timeout 1000000 $ putStrLn $ nonterminatingEquality `seq` "Now forcing the equality..."
  case result of
    Just _ -> do
      putStrLn "Singleton incorrectly optimized"
      exitFailure
    Nothing -> do
      putStrLn "Singleton correctly left unoptimized"
      exitSuccess

