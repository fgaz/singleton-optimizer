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
, UnsafeTotal(..)
) where

import GhcPlugins hiding ((<>), isSingleton) -- MAYBE make explicit
import Data.Functor
  ( ($>) )
import Control.Monad
  ( (<=<), filterM )
import Data.Data
  ( Data )
import Data.Maybe
  ( isJust )
import PrelNames
  ( dollarIdKey, seqIdKey, traceKey
  , plusNaturalIdKey, plusIntegerIdKey, minusNaturalIdKey, minusIntegerIdKey )

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

-- | use this annotation to manually mark a binding as total. It will /not/ be
-- automatically checked for totality and it will be unconditionally treated as
-- total.
data UnsafeTotal = UnsafeTotal deriving (Data, Show)

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
  let isMarkedUnsafeTotal b = do
        anns <- annotationsOn g b :: CoreM [UnsafeTotal]
        pure $ not $ null anns
  unsafelyTotalBinds <- filterM (isMarkedUnsafeTotal . fst) $ flattenBinds $ mg_binds g
  let nonTerm = foldMap terminationVars infos
      locallyTotalBinds = filter ((`notElem` nonTerm) . fst) (flattenBinds $ mg_binds g)
                       <> unsafelyTotalBinds
      -- This allows for only one level of indirection, but it will have to do
      -- for now.
      -- TODO investigate what exactly I have to check if I want to allow n
      -- indirections, since Rec binds are probably already checked by LH.
      definitelyTotalBinders = fst <$> filter (\(b, e) -> null $ externalReferences (b : fmap fst unsafelyTotalBinds) e) locallyTotalBinds <> unsafelyTotalBinds
      -- TEMP two temporary additional levels of indirection
      definitelyTotalBinders' = fst <$> filter (\(b, e) -> null $ externalReferences (b:definitelyTotalBinders) e) locallyTotalBinds <> unsafelyTotalBinds
      definitelyTotalBinders'' = fst <$> filter (\(b, e) -> null $ externalReferences (b:definitelyTotalBinders') e) locallyTotalBinds <> unsafelyTotalBinds
  newBinds <- mapM
                (mapBind $ optimizeAnnotatedSingleton g definitelyTotalBinders'')
                (mg_binds g)
  pure g { mg_binds = newBinds }

-- Should mostly work if the UnhelpfulSpan is always a relative path (MAYBE check it?)
fromSrcSpan :: SrcSpan -> FilePath
fromSrcSpan (UnhelpfulSpan fs) = unpackFS fs
fromSrcSpan (RealSrcSpan rss) = unpackFS $ srcSpanFile rss

-- | Substituted all singletons with a certain annotation with a no-op
optimizeAnnotatedSingleton :: ModGuts
                           -> [CoreBndr] -- ^ definitely total binders
                           -> (CoreBndr, CoreExpr)
                           -> CoreM (CoreBndr, CoreExpr)
optimizeAnnotatedSingleton guts totalBinders (b, expr) = do
  anns <- annotationsOn guts b :: CoreM [OptimizeSingleton]
  let bt = varType b -- TODO normalize type synonyms
      singl = isSingleton bt
      tot = b `elem` totalBinders
      ext = externalReferences (b:totalBinders) expr
  case (anns, singl, tot  , ext) of
       (_:_ , True , True , [] ) -> pure (b, coercedSingleton bt)
       ([]  , _    , _    , _  ) -> pure (b, expr)
       (_:_ , False, _    , _  ) -> explainFailure NotASingleton b $> (b, expr)
       (_:_ , _    , _    , _:_) -> explainFailure (ContainsExternalReferences ext) b $> (b, expr)
       (_:_ , _    , False, _  ) -> explainFailure NotProvedTotal b $> (b, expr)

data Failure = NotASingleton
             | NotProvedTotal
             | ContainsExternalReferences [Var]

explainFailure :: Failure -> CoreBndr -> CoreM ()
explainFailure failure b = warnMsg $
  "Cannot optimize" <+> ppr b
  <+> "because:" <+> failureExplaination failure

failureExplaination :: Failure -> SDoc
failureExplaination NotASingleton =
  "Not a singleton"
failureExplaination NotProvedTotal =
  "Not proved total by Liquid haskell"
failureExplaination (ContainsExternalReferences vars) =
  "Contains references to things other than the function itself,"
  <+> "total functions internal to the module,"
  <+> "data constructors,"
  <+> "and a few whitelisted functions ($, +, -, seq, trace...)."
  $+$ "External references: " <+> ppr vars

-- | Known-total external identifiers
whitelistedIds :: [Unique]
whitelistedIds =
  [ dollarIdKey
  , seqIdKey
  , traceKey
  , plusNaturalIdKey
  , plusIntegerIdKey
  , minusNaturalIdKey
  , minusIntegerIdKey ]

externalReferences :: [CoreBndr] -- ^ Allowed identifiers
                   -> CoreExpr -- ^ Expression to check
                   -> [Var] -- ^ External (and not allowed) 'Var's
externalReferences bs (Var var) = [var | external]
  where
    external = not inAllowedBinders
            && not isDataConstructor
            && not isWhitelisted
    inAllowedBinders = var `elem` bs
    -- for some reason `isDataConName $ varName var` doesn't work but this does:
    isDataConstructor = isJust $ isDataConId_maybe var
    isWhitelisted = varUnique var `elem` whitelistedIds
externalReferences _  (Lit _) = []
externalReferences bs (App e e') =
  externalReferences bs e
  <> externalReferences bs e'
externalReferences bs (Lam b e) = externalReferences (b:bs) e
externalReferences bs (Let (NonRec b e') e) =
  externalReferences bs e' -- not necessary to do b:bs because it's nonrec
  <> externalReferences (b:bs) e
externalReferences bs (Let (Rec binders) e) =
  binderRefs
  <> externalReferences (binderBound <> bs) e
  where
    binderBound = fst <$> binders
    binderRefs = foldMap (externalReferences (binderBound <> bs) . snd) binders
externalReferences bs (Case e b _ alts) =
  externalReferences bs e
  <> foldMap altRefs alts
  where
    altRefs (_, altBinds, altExpr) = externalReferences (b : altBinds <> bs) altExpr
externalReferences bs (Cast e _) = externalReferences bs e
externalReferences bs (Tick _ e) = externalReferences bs e
externalReferences _  (Type _) = []
externalReferences _  (Coercion _) = []


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

