{-# LANGUAGE LambdaCase, QuasiQuotes, TupleSections #-}
import Data.Array
import Data.List
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
getByDelta arr (dx, dy) = unfoldr (\(ix, iy) -> fmap (,(ix+dx, iy+dy)) (arr!?(ix, iy)))

makeMove :: GameState -> Move -> Either String GameState
makeMove (GameState curPlayer board) = aux where
    aux (Move src _) | not (inBounds src) = Left $ [s|Source %? is out of bounds|] src
    aux (Move _ dst) | not (inBounds dst) = Left $ [s|Destination %? is out of bounds|] dst
    aux (Move src@(x1, y1) dst@(x2, y2)) = maybe (Left $ [s|No piece is at position %?|] src) Right (board!src) >>= \case
        ChessPiece _ col _ | col /= curPlayer -> Left $ [s|%? has %?'s piece, and it's %?'s turn|] src col curPlayer
        ChessPiece Pawn col moved -> if (y2 `elem` map (y1+) (pawnMoveSet col moved))
            -- TODO: promotion, en passant
            then Right (GameState (otherColor curPlayer) (board // [(src, Nothing), (dst, Just $ ChessPiece Pawn col True)]))
            else Left "Invalid move."
        _ -> error "Not yet implemented."
    pawnMoveSet col moved = map (case col of {Black -> negate; White -> id}) (if moved then [1] else [1,2])
    inBounds = inRange ((1,1), (8, 8))
