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


import GHC.Plugin.SingletonOptimizer.Whitelist
  ( getWhitelistedArgTypes, getWhitelistedVarNames )

import GhcPlugins hiding ((<>), isSingleton) -- MAYBE make explicit
import Data.Functor
  ( ($>) )
import Control.Monad
  ( (<=<), filterM )
import Data.Data
  ( Data )
import Data.Maybe
  ( isJust )

import qualified Language.Haskell.Liquid.UX.Config as LH.Config
import Language.Haskell.Liquid.UX.CmdLine
  ( mkOpts, defConfig )
import Language.Haskell.Liquid.GHC.Interface
  ( getGhcInfos )
import Language.Haskell.Liquid.Termination.Structural
  ( terminationVars )


-- | All singletons --or functions that take as input only whitelisted elements
-- and return singletons-- marked with this annotation will be checked for
-- totality and, in case, optimized.
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
  { installCoreToDos = installCorePlugin
  , pluginRecompile = purePlugin }

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
  wns <- getWhitelistedVarNames
  let nonTerm = foldMap terminationVars infos
      locallyTotalBinds = filter ((`notElem` nonTerm) . fst) (flattenBinds $ mg_binds g)
                       <> unsafelyTotalBinds
      -- This allows for only one level of indirection, but it will have to do
      -- for now.
      -- TODO investigate what exactly I have to check if I want to allow n
      -- indirections, since Rec binds are probably already checked by LH.
      definitelyTotalBinders = fst <$> filter (\(b, e) -> null $ externalReferences wns (b : fmap fst unsafelyTotalBinds) e) locallyTotalBinds <> unsafelyTotalBinds
      -- TEMP two temporary additional levels of indirection
      definitelyTotalBinders' = fst <$> filter (\(b, e) -> null $ externalReferences wns (b:definitelyTotalBinders) e) locallyTotalBinds <> unsafelyTotalBinds
      definitelyTotalBinders'' = fst <$> filter (\(b, e) -> null $ externalReferences wns (b:definitelyTotalBinders') e) locallyTotalBinds <> unsafelyTotalBinds
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
  allowedArgs <- getWhitelistedArgTypes
  wns <- getWhitelistedVarNames
  let bt = varType b -- TODO normalize type synonyms
      singl = isSingleton allowedArgs bt
      tot = b `elem` totalBinders
      ext = externalReferences wns (b:totalBinders) expr
  case (anns, singl, tot  , ext) of
       (_:_ , True , True , [] ) -> pure (b, coercedSingleton expr)
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

-- MAYBE merge the two whitelists since there's a `CoreBndr -> Name` function
externalReferences :: [Name] -- ^ Whitelisted names (module-external)
                   -> [CoreBndr] -- ^ Allowed identifiers (module-internal)
                   -> CoreExpr -- ^ Expression to check
                   -> [Var] -- ^ External (and not allowed) 'Var's
externalReferences wns bs (Var var) = [var | external]
  where
    external = not inAllowedBinders
            && not isDataConstructor
            && not isDFun
            && not isWhitelisted
    inAllowedBinders = var `elem` bs
    -- for some reason `isDataConName $ varName var` doesn't work but this does:
    isDataConstructor = isJust $ isDataConId_maybe var
    isWhitelisted = varName var `elem` wns
    isDFun = isDFunId var
externalReferences _ _  (Lit _) = []
externalReferences wns bs (App e e') =
  externalReferences wns bs e
  <> externalReferences wns bs e'
externalReferences wns bs (Lam b e) = externalReferences wns (b:bs) e
externalReferences wns bs (Let (NonRec b e') e) =
  externalReferences wns bs e' -- not necessary to do b:bs because it's nonrec
  <> externalReferences wns (b:bs) e
externalReferences wns bs (Let (Rec binders) e) =
  binderRefs
  <> externalReferences wns (binderBound <> bs) e
  where
    binderBound = fst <$> binders
    binderRefs = foldMap (externalReferences wns (binderBound <> bs) . snd) binders
externalReferences wns bs (Case e b _ alts) =
  externalReferences wns bs e
  <> foldMap altRefs alts
  where
    altRefs (_, altBinds, altExpr) = externalReferences wns (b : altBinds <> bs) altExpr
externalReferences wns bs (Cast e _) = externalReferences wns bs e
externalReferences wns bs (Tick _ e) = externalReferences wns bs e
externalReferences _ _  (Type _) = []
externalReferences _ _  (Coercion _) = []

-- | Check whether a 'Type.Type' is a singleton (has a single inhabitant)
-- or is a function that can return a singleton (every argument is inhabited).
-- This does not catch all singleton cases, but it will definitely return
-- 'False' when the type is NOT a singleton, so we're good.
-- Cases where it returns 'False' on singletons:
-- * @data SomeType = SomeConstructor SomeSingleton@
-- * probably others
isSingleton :: [Name] -> Type -> Bool
isSingleton allowedArgs t = resIsSingleton res
                         && all isAllowedArg args
  where
    (_foralls, t') = splitForAllTys t
    (args, res) = splitFunTys t'
    resIsSingleton = maybe False (null . dataConOrigArgTys) .
                       (tyConSingleDataCon_maybe <=< tyConAppTyCon_maybe)
    isAllowedArg at = fmap tyConName (tyConAppTyCon_maybe at)
                      `elem`
                      fmap Just allowedArgs

-- | Construct the only possible inhabitant of a singleton, replacing the given
-- expression, but keeping its arity and the type of its arguments intact.
-- MUST be called on singletons (or functions that can return a singleton) only
coercedSingleton :: CoreExpr -> CoreExpr
coercedSingleton originalExpr = newExpr
  where
    newExpr =
      case originalExpr of
        -- Keep the arity
        Lam b e -> Lam b $ coercedSingleton e
        e -> coercedConstructor $ exprType e
    coercedConstructor t =
      case tyConAppTyCon_maybe t >>= tyConSingleDataCon_maybe of
        Nothing -> error $ "Error: trying to optimize a non-singleton "
                        ++ "(this is a bug, please report it)."
        Just dataCon ->
          -- Find the only possible constructor of the singleton
          let singletonConstructor = dataConWrapId dataCon
          -- Coerce that constructor to the appropriate type
          in Cast
              (Var singletonConstructor)
              $ mkUnsafeCo
                  Representational
                  (varType singletonConstructor)
                  t

-- | Map a function to both 'Rec' and 'NonRec' bindings
mapBind :: ((b, Expr b) -> CoreM (b, Expr b)) -> Bind b -> CoreM (Bind b)
mapBind f (NonRec b expr) = uncurry NonRec <$> f (b, expr)
mapBind f (Rec bs) = Rec <$> sequenceA (f <$> bs)

-- | Find all annotations on a specific binding
annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  pure $ lookupWithDefaultUFM anns [] (varUnique bndr)

