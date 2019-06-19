module Snippets where

import GhcPlugins hiding ((<>), isSingleton)
import qualified GhcPlugins as P

info :: SDoc -> CoreM () -- TODO handle verbosity
info = putMsg

-- Substitute this to optimiseAnnotatedSingleton for debugging
printInfo :: ModGuts
          -> [Var]
          -> (CoreBndr, CoreExpr)
          -> CoreM (CoreBndr, CoreExpr)
printInfo guts _nonTerm (b, expr) = do
  anns <- annotationsOn guts b :: CoreM [OptimizeSingleton]
  info $ "Annotations on" <+> ppr b P.<> ":" <+> text (show anns)
  info $ "The expr of" <+> ppr b <+> "is:" <+> ppr expr
  info $ "The type of" <+> ppr b <+> "is:" <+> ppr (varType b)
  info $ "The splitted type of" <+> ppr b <+> "is:" <+> ppr (splitTyConApp_maybe $ varType b)
  let singl = isSingleton $ varType b
  info $ "The binding" <+> ppr b <+> "is a singleton:" <+> ppr singl
  unless (null anns) $ info $ "Annotated binding found:" <+> ppr b
  pure (b, expr)

