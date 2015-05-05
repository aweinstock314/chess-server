module ChessUtil where
import Language.Haskell.TH

fileLiteral = runIO . fmap (LitE . StringL) . readFile

onFailDo (Left _) x = x
onFailDo x _ = x
