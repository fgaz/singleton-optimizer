{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Plugin.SingletonOptimizer
( plugin
, OptimizeSingleton(..)
) where

-- TODO choose english vs american zpelling :-P

-- TODO explicit/qualified imports to avoid future breakage
import GhcPlugins hiding ((<>), isSingleton)
import qualified GhcPlugins as P
import qualified TcRnTypes as T
import Control.Monad (unless)
import Data.Data (Data)


info :: SDoc -> CoreM () -- TODO handle verbosity
info = putMsg

-- | All singletons marked with this annotation will be optimized
data OptimizeSingleton = OptimizeSingleton deriving (Data, Show)

-- | The singleton-optimizer plugin.
-- Right now it blindly optimizes all marked singletons.
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installCorePlugin
  , typeCheckResultAction = callLH }

installCorePlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installCorePlugin _ todo =
  pure $ CoreDoPluginPass "Optimize total singletons" pass : todo

pass :: ModGuts -> CoreM ModGuts
pass g = do
  dflags <- getDynFlags
  newBinds <- mapM (mapBind (optimiseAnnotatedSingleton dflags g)) (mg_binds g)
  pure g { mg_binds = newBinds }

-- Substitute this to optimiseAnnotatedSingleton for debugging
printInfo :: DynFlags
          -> ModGuts
          -> (CoreBndr, CoreExpr)
          -> CoreM (CoreBndr, CoreExpr)
printInfo _dflags guts (b, expr) = do
  anns <- annotationsOn guts b :: CoreM [OptimizeSingleton]
  info $ "Annotations on" <+> ppr b P.<> ":" <+> text (show anns)
  info $ "The expr of" <+> ppr b <+> "is:" <+> ppr expr
  info $ "The type of" <+> ppr b <+> "is:" <+> ppr (varType b)
  info $ "The splitted type of" <+> ppr b <+> "is:" <+> ppr (splitTyConApp_maybe $ varType b)
  let singl = isSingleton $ varType b
  info $ "The binding" <+> ppr b <+> "is a singleton:" <+> ppr singl
  unless (null anns) $ info $ "Annotated binding found:" <+> ppr b
  pure (b, expr)

-- | Substituted all singletons with a certain annotation with a no-op
optimiseAnnotatedSingleton :: DynFlags
                           -> ModGuts
                           -> (CoreBndr, CoreExpr)
                           -> CoreM (CoreBndr, CoreExpr)
optimiseAnnotatedSingleton _dflags guts (b, expr) = do
  anns <- annotationsOn guts b :: CoreM [OptimizeSingleton]
  let singl = isSingleton $ varType b -- TODO normalize type synonyms
  pure $ case (anns, singl) of
    (_:_, True) -> (b, coercedSingleton $ varType b)
    _           -> (b, expr)

-- | Check whether a 'Type' is a singleton (has a single inhabitant).
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

-- TODO this should add some 'ThisIsTotalOptimizeIt' annotation
--      or a global annotation with a list of 'Var's
-- TODO can we put this in the Core layer?
--      Do we have the filename info there (maybe through HscEnv)?
--      Or can we reduce the amount of needed info? The API exposed by LH is
--      pretty limited...
callLH :: [CommandLineOption] -> ModSummary -> T.TcGblEnv -> T.TcM T.TcGblEnv
callLH _ _ = pure

