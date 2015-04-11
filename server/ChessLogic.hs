{-# LANGUAGE LambdaCase, QuasiQuotes, TupleSections #-}
import Control.Arrow
import Data.Array
import Data.List
import Data.Maybe
import Text.Printf.TH

type Location = (Int, Int)
data Move = Move { mvSource :: Location, mvDest :: Location } deriving Show
type ChessBoard = Array Location (Maybe ChessPiece)
data GameState = GameState {
    gsCurrentPlayer :: ChessPieceColor,
    gsBoard :: ChessBoard
    }

data ChessPieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq, Show)
data ChessPieceColor = Black | White deriving (Eq, Show)

data ChessPiece = ChessPiece {
    cpType :: ChessPieceType,
    cpColor :: ChessPieceColor,
    cpHasMoved :: Bool
    }

instance Show ChessPiece where show (ChessPiece ty col _) = [s|ChessPiece %6? %?|] ty col

defaultBoard = listArray ((1, 1), (8, 8)) . concat $ transpose [
    [mk Rook White, mk Knight White, mk Bishop White, mk Queen White, mk King White, mk Bishop White, mk Knight White, mk Rook White],
    [mk Pawn White, mk   Pawn White, mk   Pawn White, mk  Pawn White, mk Pawn White, mk   Pawn White, mk   Pawn White, mk Pawn White],
    [Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing],
    [Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing],
    [Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing],
    [Nothing,               Nothing,         Nothing,        Nothing,       Nothing,         Nothing,         Nothing,       Nothing],
    [mk Pawn Black, mk   Pawn Black, mk   Pawn Black, mk  Pawn Black, mk Pawn Black, mk   Pawn Black, mk   Pawn Black, mk Pawn Black],
    [mk Rook Black, mk Knight Black, mk Bishop Black, mk Queen Black, mk King Black, mk Bishop Black, mk Knight Black, mk Rook Black]
    ] where mk piece color = Just $ ChessPiece piece color False

otherColor Black = White
otherColor White = Black

arr !? i = if inRange (bounds arr) i then Just (arr!i) else Nothing
getByDelta arr (dx, dy) = unfoldr (\i@(ix, iy) -> fmap ((,(ix+dx, iy+dy)) . (i,)) (arr !? i)) . ((+dx) *** (+dy))

makeMove :: GameState -> Move -> Either String GameState
makeMove (GameState curPlayer board) = aux where
    aux (Move src _) | not (inBounds src) = Left $ [s|Source %? is out of bounds|] src
    aux (Move _ dst) | not (inBounds dst) = Left $ [s|Destination %? is out of bounds|] dst
    aux move@(Move src@(x1, y1) dst@(x2, y2)) = maybe (Left $ [s|No piece is at position %?|] src) Right (board!src) >>= \case
        ChessPiece _ col _ | col /= curPlayer -> Left $ [s|%? has %?'s piece, and it's %?'s turn|] src col curPlayer
        piece@(ChessPiece Pawn col moved) -> moveIfInSet move piece (map ((x1,) . (y1+)) (pawnMoveSet col moved)) -- TODO: promotion, en passant
        piece@(ChessPiece Rook _ _) -> straightLineMovement orthogonalDeltas move piece
        piece@(ChessPiece Bishop _ _) -> straightLineMovement diagonalDeltas move piece
        piece@(ChessPiece Queen _ _) -> straightLineMovement (orthogonalDeltas ++ diagonalDeltas) move piece
        _ -> error "Not yet implemented."
    pawnMoveSet col moved = map (case col of {Black -> negate; White -> id}) (if moved then [1] else [1,2])
    straightLineMovement deltas move@(Move src dst) piece = moveIfInSet move piece $
        concatMap (\delta -> map fst . takeWhileUnoccupied $ getByDelta board delta src) deltas
    takeWhileUnoccupied = fst . foldr (\(i,e) (a, done) -> (if done then a else (i,e):a, done || isNothing e)) ([], False)
    orthogonalDeltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    diagonalDeltas = [(-1, -1), (-1, 1), (1, 1), (1, -1)]
    moveIfInSet move piece set = if mvDest move `elem` set then uncheckedMakeMove move piece else Left "Invalid move"
    uncheckedMakeMove (Move src dst) piece = Right (GameState (otherColor curPlayer) (board // [(src, Nothing), (dst, Just (piece {cpHasMoved=True}))]))
    inBounds = inRange ((1,1), (8, 8))
