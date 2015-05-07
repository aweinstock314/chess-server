{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
import ChessLogic
import ChessUtil
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Array
import Data.IORef
import Text.Printf.TH
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as HWS
import qualified Network.WebSockets as WS

instance (Ix a, A.ToJSON a, A.ToJSON b) => A.ToJSON (Array a b) where toJSON arr = A.toJSON (bounds arr, assocs arr)
instance (Ix a, A.FromJSON a, A.FromJSON b) => A.FromJSON (Array a b) where parseJSON = fmap (uncurry array) . A.parseJSON

data ClientCommand = SubmitMove Location | ClientDisconnected
data ServerCommand = DisplayGameState GameState | DisplayValidMoves (Array Location Bool)
    | DisplayPlayerID ChessPieceColor | DisplayWhoseTurn ChessPieceColor
    | DisplayOpponentDisconnected | DisplayInCheck
    | CheckmateHappened ChessPieceColor | StalemateHappened
    | EnableInput | DisableInput

fmap concat $ mapM (AT.deriveJSON AT.defaultOptions) [''ClientCommand, ''ServerCommand]

htmlPage = $(fileLiteral "../client/index.html")

httpServer :: Wai.Application
httpServer request respond = respond $ Wai.responseLBS HTTP.status200 [] htmlPage

waitingRoom :: MVar (Chan ServerCommand, Chan (Maybe ClientCommand)) -> WS.ServerApp
waitingRoom waitList pendingConn = do
    sender <- newChan
    receiver <- newChan
    conn <- WS.acceptRequest pendingConn
    WS.forkPingThread conn 20
    tryTakeMVar waitList >>= \case
        Just (sender, receiver) -> playGame conn (sender, receiver)
        Nothing -> putMVar waitList (sender, receiver) >> do
            handle (\e -> (e :: WS.ConnectionException) `seq` writeChan receiver (Just ClientDisconnected)) $ do
                WS.sendTextData conn . A.encode $ DisplayPlayerID White
                forkIO . forever $ readChan sender >>= WS.sendTextData conn . A.encode
                forever $ do
                    msg <- fmap A.decode $ WS.receiveData conn >>= showReceived White
                    writeChan receiver msg

showReceived player msg = putStrLn ([s|Received from %?: %?|] player msg) >> return msg

playGame conn (sender, receiver) = handle (\e -> (e :: WS.ConnectionException) `seq` writeChan sender DisplayOpponentDisconnected) $ do
    let broadcast msg = do
        writeChan sender msg
        WS.sendTextData conn $ A.encode msg
    let { singlecast White msg = writeChan sender msg; singlecast Black msg = WS.sendTextData conn $ A.encode msg }
    let { getMessage White = readChan receiver; getMessage Black = fmap A.decode $ WS.receiveData conn >>= showReceived Black }
    currentGameState <- newIORef defaultGameState
    lastLocClicked <- newIORef Nothing
    WS.sendTextData conn . A.encode $ DisplayPlayerID Black
    readIORef currentGameState >>= \gs -> broadcast $ DisplayGameState gs
    let loop = do
        gs <- readIORef currentGameState
        broadcast $ DisplayWhoseTurn (gsCurrentPlayer gs)
        singlecast (gsCurrentPlayer gs) EnableInput
        msg <- getMessage (gsCurrentPlayer gs)
        singlecast (gsCurrentPlayer gs) DisableInput
        case msg of
            Just (SubmitMove dst) -> do
                readIORef lastLocClicked >>= \case
                    Nothing -> do
                        singlecast (gsCurrentPlayer gs) $ DisplayGameState gs
                        singlecast (gsCurrentPlayer gs) $ DisplayValidMoves (validMoves gs dst)
                        writeIORef lastLocClicked $ Just dst
                        loop
                    Just src -> case abortIfInCheck $ makeMove gs (Move src dst) of
                        Left errmsg -> do
                            singlecast (gsCurrentPlayer gs) $ DisplayGameState gs
                            singlecast (gsCurrentPlayer gs) $ DisplayValidMoves (validMoves gs dst)
                            writeIORef lastLocClicked $ Just dst -- TODO: maybe give the user the error message?
                            loop
                        Right newGameState -> do
                            writeIORef currentGameState newGameState
                            writeIORef lastLocClicked Nothing
                            broadcast $ DisplayGameState newGameState
                            unless (not $ anyValidMovesExist newGameState) $ do
                                when (inCheck newGameState (gsCurrentPlayer newGameState)) $
                                    singlecast (gsCurrentPlayer newGameState) DisplayInCheck
                                loop
                            when (not $ anyValidMovesExist newGameState) $ do
                                if inCheck newGameState (gsCurrentPlayer newGameState)
                                    then broadcast $ CheckmateHappened (gsCurrentPlayer gs)
                                    else broadcast StalemateHappened
            Just ClientDisconnected -> WS.sendTextData conn . A.encode $ DisplayOpponentDisconnected
            Nothing -> return ()
    loop

main = do
    waitList <- newEmptyMVar
    let portNumber = 8000
    [sP|Listening on port %d.|] portNumber
    Warp.run portNumber (HWS.websocketsOr WS.defaultConnectionOptions (waitingRoom waitList) httpServer)
