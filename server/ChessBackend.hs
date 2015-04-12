{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
import ChessLogic
import ChessUtil
import Control.Monad
import Data.Array
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

data ClientCommand = RequestValidMoves Location | SomethingElse
data ServerCommand = DisplayGameState GameState | RespondValidMoves (Array Location Bool)

fmap concat $ mapM (AT.deriveJSON AT.defaultOptions) [''ClientCommand, ''ServerCommand]

htmlPage = $(fileLiteral "../client/index.html")

httpServer :: Wai.Application
httpServer request respond = respond $ Wai.responseLBS HTTP.status200 [] htmlPage

websocketServer :: WS.ServerApp
websocketServer pending = do
    sock <- WS.acceptRequest pending
    WS.sendTextData sock (A.encode $ DisplayGameState defaultGameState)
    forever $ do
        msg <- fmap A.decode $ WS.receiveData sock
        case msg of
            Just (RequestValidMoves loc) -> do
                WS.sendTextData sock (A.encode $ RespondValidMoves (validMoves defaultGameState loc))
            Nothing -> return ()

main = Warp.run 8000 (HWS.websocketsOr WS.defaultConnectionOptions websocketServer httpServer)
