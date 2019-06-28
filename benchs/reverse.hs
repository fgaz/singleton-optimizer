{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wno-unused-local-binds -Wno-unused-top-binds #-}

module Main (main) where

import GHC.Plugin.SingletonOptimizer ( OptimizeSingleton(OptimizeSingleton) )
import GHC.Plugin.SingletonOptimizer.Whitelist.Types (N(..), SN(..))
import Data.Type.Equality ((:~:)(Refl))

import Criterion.Main

snInt :: SN n -> Int
snInt SZ     = 0
snInt (SS n) = succ (snInt n)

type family (n :: N) :+: (m :: N) :: N where
    'Z   :+: m = m
    'S n :+: m = 'S (n :+: m)

type ProofIdentityZ n = SN n -> n :+: 'Z :~: n

{-# ANN proofIdentityZ OptimizeSingleton #-}
proofIdentityZ :: ProofIdentityZ n
proofIdentityZ SZ = Refl
proofIdentityZ (SS n) = case proofIdentityZ n of Refl -> Refl

proofIdentityZUnoptimized :: ProofIdentityZ n
proofIdentityZUnoptimized SZ = Refl
proofIdentityZUnoptimized (SS n) = case proofIdentityZUnoptimized n of Refl -> Refl

type ProofS n m = SN n -> SN m -> n :+: 'S m :~: 'S (n :+: m)

{-# ANN proofS OptimizeSingleton #-}
proofS :: ProofS n m
proofS SZ     _ = Refl
proofS (SS n) m = case proofS n m of Refl -> Refl

proofSUnoptimized :: ProofS n m
proofSUnoptimized SZ     _ = Refl
proofSUnoptimized (SS n) m = case proofSUnoptimized n m of Refl -> Refl

-------------------------------------

data Vec a (n :: N) where
    Nil  :: Vec a 'Z
    Cons :: a -> Vec a n -> Vec a ('S n)

lengthVec :: Vec a n -> SN n
lengthVec Nil         = SZ
lengthVec (Cons _ xs) = SS (lengthVec xs)

reverseVecIt :: Vec a n -> Vec a n
reverseVecIt v =
    case proofIdentityZ (lengthVec v) of
      Refl -> revWrap v Nil

reverseVecItUnoptimized :: Vec a n -> Vec a n
reverseVecItUnoptimized v =
    case proofIdentityZUnoptimized (lengthVec v) of
      Refl -> revWrapUnoptimized v Nil

revWrap :: Vec a n -> Vec a m -> Vec a (n :+: m)
revWrap Nil         acc = acc
revWrap (Cons x xs) acc =
    case proofS (lengthVec xs) (lengthVec acc) of
      Refl -> revWrap xs (Cons x acc)

revWrapUnoptimized :: Vec a n -> Vec a m -> Vec a (n :+: m)
revWrapUnoptimized Nil         acc = acc
revWrapUnoptimized (Cons x xs) acc =
    case proofSUnoptimized (lengthVec xs) (lengthVec acc) of
      Refl -> revWrapUnoptimized xs (Cons x acc)

-----------------------------------

data VecSomeLength a where
    VSL :: Vec a n -> VecSomeLength a

longVecSomeLenght :: Int -> VecSomeLength Int
longVecSomeLenght 0 = VSL Nil
longVecSomeLenght n =
    case longVecSomeLenght (n - 1) of
      VSL v -> VSL (Cons n v)

main :: IO ()
main = defaultMain
  [ bgroup "unoptimized" $
      (\n -> bench (show n) $ whnf fu $ longVecSomeLenght n) <$> [10000,11000..40000]
  , bgroup "optimized" $
      (\n -> bench (show n) $ whnf fo $ longVecSomeLenght n) <$> [10000,11000..40000]
  ]
  where
    fu (VSL w) = snInt . lengthVec $ reverseVecItUnoptimized w
    fo (VSL w) = snInt . lengthVec $ reverseVecIt w

