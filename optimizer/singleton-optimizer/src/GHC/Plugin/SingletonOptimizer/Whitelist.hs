{-# LANGUAGE TemplateHaskell #-}

module GHC.Plugin.SingletonOptimizer.Whitelist
( getWhitelistedArgTypes
, getWhitelistedVarNames
) where

import GHC.Plugin.SingletonOptimizer.Whitelist.Types

import GhcPlugins
  ( CoreM, Name, Unique, thNameToGhcName, nameUnique )

import Data.Maybe
  ( catMaybes )
import Debug.Trace
  ( trace )
import GHC.Exts
  ( build )

-- | Names of types that are allowed as parameters of functions that return
-- a singleton
getWhitelistedArgTypes :: CoreM [Name]
getWhitelistedArgTypes = fmap catMaybes $ sequenceA $ thNameToGhcName <$>
  [ ''SN ]

-- | Known-total identifiers
getWhitelistedVarNames :: CoreM [Name]
getWhitelistedVarNames = fmap catMaybes $ sequenceA $ thNameToGhcName <$>
  -- Internally defined
  [ 'SZ
  , 'SS
  , 'Z
  , 'S
  -- Some total functions in base
  , '(.)
  , '($)
  , 'seq
  , 'trace
  , '(+)
  , '(-)
  , '(*)
  , 'build ]

