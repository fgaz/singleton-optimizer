module Helpers
( NotOptimizedException(..)
, expectOptimized
, expectUnoptimized
, reportResults
, knownFailure
) where

import Data.Functor(($>))
import Data.Maybe (fromMaybe)
import Control.Exception (Exception, NonTermination, catch, evaluate)
import System.Timeout (timeout)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

data NotOptimizedException = NotOptimizedException deriving Show

instance Exception NotOptimizedException

shouldAlsoTerminate :: IO Bool -> IO Bool
shouldAlsoTerminate f =
  catch
    (fromMaybe False <$> timeout 1000000 f)
    (const $ pure False :: NonTermination -> IO Bool)

canHang :: IO Bool -> IO Bool
canHang f =
  catch
    (fromMaybe True <$> timeout 1000000 f)
    (const $ pure True :: NonTermination -> IO Bool)

expectOptimized :: a -> IO Bool
expectOptimized singl =
  shouldAlsoTerminate $
  catch
    (evaluate singl $> True)
    (const $ pure False :: NotOptimizedException -> IO Bool)

expectUnoptimized :: a -> IO Bool
expectUnoptimized singl =
  canHang $
  catch
    (evaluate singl $> False)
    (const $ pure True :: NotOptimizedException -> IO Bool)

reportResults :: String -> IO Bool -> IO Bool
reportResults desc ioRes = do
  res <- ioRes
  let passfail =
        PP.dullblack $
          if res
          then
            PP.ondullgreen $ PP.text "PASS"
          else
            PP.ondullred $ PP.text "FAIL"
      line = passfail <> PP.colon PP.<+> PP.text desc <> PP.line
  PP.putDoc line
  pure res

-- TODO unify with reportResults
knownFailure :: String -> IO Bool -> IO Bool
knownFailure desc ioRes = do
  res <- ioRes
  let passfail =
        PP.dullblack $
          if res
          then
            PP.ondullred $ PP.text "UNEXPECTED PASS"
          else
            PP.ondullyellow $ PP.text "KNOWN FAIL"
      line = passfail <> PP.colon PP.<+> PP.text desc <> PP.line
  PP.putDoc line
  pure $ not res

