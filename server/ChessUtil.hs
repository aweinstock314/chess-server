{-# LANGUAGE NoMonomorphismRestriction #-}
module ChessUtil where
import Debug.Trace
import Language.Haskell.TH

fileLiteral = runIO . fmap (LitE . StringL) . readFile

traceId x = trace (show x) x

onFailDo (Left _) x = x
onFailDo x _ = x
