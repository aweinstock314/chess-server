{-# LANGUAGE LambdaCase, QuasiQuotes #-}
import Control.Arrow
import Data.Array
import Text.Printf.TH

type Location = (Int, Int)
data Move = Move { mvSource :: Location, mvDest :: Location } deriving Show
type ChessBoard = Array Location (Maybe ChessPiece)

data ChessPieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving Show
data ChessPieceColor = Black | White deriving Show

data ChessPiece = ChessPiece {
    cpType :: ChessPieceType,
    cpColor :: ChessPieceColor,
    cpHasMoved :: Bool
    }

instance Show ChessPiece where show (ChessPiece ty col _) = [s|ChessPiece %6? %?|] ty col

both f = (f *** f)

defaultBoard = listArray ((1, 1), (8, 8)) [
    mk Rook White, mk Knight White, mk Bishop White, mk Queen White, mk King White, mk Bishop White, mk Knight White, mk Rook White,
    mk Pawn White, mk   Pawn White, mk   Pawn White, mk  Pawn White, mk Pawn White, mk   Pawn White, mk   Pawn White, mk Pawn White,
    Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing,
    Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing,
    Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing,
    Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing,
    mk Pawn Black, mk   Pawn Black, mk   Pawn Black, mk  Pawn Black, mk Pawn Black, mk   Pawn Black, mk   Pawn Black, mk Pawn Black,
    mk Rook Black, mk Knight Black, mk Bishop Black, mk Queen Black, mk King Black, mk Bishop Black, mk Knight Black, mk Rook Black
    ] where mk piece color = Just $ ChessPiece piece color False

makeMove :: ChessBoard -> Move -> Either String ChessBoard
makeMove board = aux where
    aux (Move src _) | not (inBounds src) = Left $ [s|Source %? is out of bounds|] src
    aux (Move _ dst) | not (inBounds dst) = Left $ [s|Destination %? is out of bounds|] dst
    aux (Move (x1, y1) (x2, y2)) = maybe (Left $ [s|No piece is at position %?|] (x1, y1)) Right (board!(x1, y1)) >>= \case
        _ -> error $ "Not yet implemented."
    inBounds (x, y) = uncurry (&&) $ both (inRange (1, 8)) (x, y)
