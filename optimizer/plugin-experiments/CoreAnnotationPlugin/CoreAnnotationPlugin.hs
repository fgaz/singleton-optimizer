{-# LANGUAGE DeriveDataTypeable #-}
module CoreAnnotationPlugin (plugin, SomeAnn(..)) where
import GhcPlugins
import Control.Monad (unless)
import Data.Data

data SomeAnn = SomeAnn deriving (Data, Show)

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Plugin installed"
  return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass g = do
          dflags <- getDynFlags
          mapM_ (printAnn dflags g) (mg_binds g) >> return g
  where printAnn :: DynFlags -> ModGuts -> CoreBind -> CoreM CoreBind
        printAnn dflags guts bndr@(NonRec b _) = do
          anns <- annotationsOn guts b :: CoreM [SomeAnn]
          putMsgS $ "Anns on " ++ showSDoc dflags (ppr b) ++ ": " ++ show anns
          unless (null anns) $ putMsgS $ "Annotated binding found: " ++  showSDoc dflags (ppr b)
          return bndr
        printAnn _ _ bndr = return bndr

annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  return $ lookupWithDefaultUFM anns [] (varUnique bndr)

