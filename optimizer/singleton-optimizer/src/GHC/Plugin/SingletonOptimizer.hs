{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  GHC.Plugin.SingletonOptimizer
-- Copyright   :  (c) Francesco Gazzetta 2019
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  fgaz@fgaz.me
-- Stability   :  experimental
-- Portability :  GHC
--
-- This plugin opimizes away any top-level binding that
--
-- * is a singleton (/not/ a function that returns a singleton)
--   (not all cases are detected)
-- * is proven total
-- * is marked with the 'OptimizeSingleton' annotation
--
-- For example:
--
-- > -- TODO write a real example in which the optimization actually saves time
-- > {-# ANN optimizedEquality OptimizeSingleton #-}
-- > optimizedEquality :: SomeType :~: SomeEquivalentType
-- > optimizedEquality = -- some expensive expression that eventually returns a 'Refl'

module GHC.Plugin.SingletonOptimizer
( -- * Plugin
  plugin
  -- * Annotations
, OptimizeSingleton(..)
) where

import GhcPlugins hiding ((<>), isSingleton) -- MAYBE make explicit
import Control.Monad
  ( (<=<) )
import Data.Data
  ( Data )

import qualified Language.Haskell.Liquid.UX.Config as LH.Config
import Language.Haskell.Liquid.UX.CmdLine
  ( mkOpts, defConfig )
import Language.Haskell.Liquid.GHC.Interface
  ( getGhcInfos )
import Language.Haskell.Liquid.Termination.Structural
  ( terminationVars )


-- | All singletons (/not/ functions that return singletons) marked with this
-- annotation will be checked for totality and, in case, optimized.
data OptimizeSingleton = OptimizeSingleton deriving (Data, Show)

-- | The singleton-optimizer plugin.
-- This gets automatically picked up by GHC when using
-- @-fplugin GHC.Plugin.SingletonOptimizer@.
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installCorePlugin }

installCorePlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installCorePlugin _ todo =
  pure $ CoreDoPluginPass "Optimize total singletons" pass : todo

pass :: ModGuts -> CoreM ModGuts
pass g = do
  env <- getHscEnv
  let files = [fromSrcSpan $ mg_loc g]
  opts <- liftIO $ mkOpts defConfig { LH.Config.files = files }
  (infos, _env') <- liftIO $ getGhcInfos (Just env) opts files -- TODO is it ok to discard env'?
  let nonTerm = foldMap terminationVars infos
  newBinds <- mapM (mapBind (optimizeAnnotatedSingleton g nonTerm)) (mg_binds g)
  pure g { mg_binds = newBinds }

-- Should mostly work if the UnhelpfulSpan is always a relative path (MAYBE check it?)
fromSrcSpan :: SrcSpan -> FilePath
fromSrcSpan (UnhelpfulSpan fs) = unpackFS fs
fromSrcSpan (RealSrcSpan rss) = unpackFS $ srcSpanFile rss

-- | Substituted all singletons with a certain annotation with a no-op
optimizeAnnotatedSingleton :: ModGuts
                           -> [Var] -- ^ non-terminating 'Var.Var's
                           -> (CoreBndr, CoreExpr)
                           -> CoreM (CoreBndr, CoreExpr)
optimizeAnnotatedSingleton guts nonTerm (b, expr) = do
  anns <- annotationsOn guts b :: CoreM [OptimizeSingleton]
  let bt = varType b -- TODO normalize type synonyms
      singl = isSingleton bt
      term = b `notElem` nonTerm
  pure $ case (anns, singl, term) of
              (_:_ , True , True) -> (b, coercedSingleton bt)
              _                   -> (b, expr)

-- | Check whether a 'Type.Type' is a singleton (has a single inhabitant).
-- This does not catch all singleton cases, but it will definitely return
-- 'False' when the type is NOT a singleton, so we're good.
-- Cases where it returns 'False' on singletons:
-- * @data SomeType = SomeConstructor SomeSingleton@
-- * probably others
isSingleton :: Type -> Bool
isSingleton = maybe False (null . dataConOrigArgTys) .
                (tyConSingleDataCon_maybe <=< tyConAppTyCon_maybe)

-- | Construct the only possible inhabitant of a singleton.
-- MUST be called on singletons only
coercedSingleton :: Type -> CoreExpr
coercedSingleton t =
  case tyConAppTyCon_maybe t >>= tyConSingleDataCon_maybe of
    Nothing -> error "Error: not a singleton"
    Just dataCon -> Var $ mkCoVar (dataConName dataCon) t

-- | Map a function to both 'Rec' and 'NonRec' bindings
mapBind :: ((b, Expr b) -> CoreM (b, Expr b)) -> Bind b -> CoreM (Bind b)
mapBind f (NonRec b expr) = uncurry NonRec <$> f (b, expr)
mapBind f (Rec bs) = Rec <$> sequenceA (f <$> bs)

-- | Find all annotations on a specific binding
annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  pure $ lookupWithDefaultUFM anns [] (varUnique bndr)

