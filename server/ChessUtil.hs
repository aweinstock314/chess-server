{-# LANGUAGE NoMonomorphismRestriction #-}
module ChessUtil where
import Debug.Trace
import Language.Haskell.TH
import System.Environment

fileLiteral = runIO . fmap (LitE . StringL) . readFile

readMaybe = foldr const Nothing . map (Just . fst) . reads

getEnvMaybe x = fmap (lookup x) getEnvironment

traceId x = trace (show x) x

onFailDo (Left _) x = x
onFailDo x _ = x
