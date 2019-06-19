{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module GHC.Plugin.SingletonOptimizer.Whitelist.Types
( N(..)
, SN(..)
) where

data N = Z | S N

data SN (n :: N) where
    SZ :: SN 'Z
    SS :: SN n -> SN ('S n)

