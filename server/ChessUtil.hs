module ChessUtil where
import Language.Haskell.TH

fileLiteral = runIO . fmap (LitE . StringL) . readFile
