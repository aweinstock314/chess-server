{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, TemplateHaskell #-}
import ChessLogic
import ChessUtil
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as HWS
import qualified Network.WebSockets as WS

htmlPage = $(fileLiteral "../client/index.html")

httpServer :: Wai.Application
httpServer request respond = respond $ Wai.responseLBS HTTP.status200 [] htmlPage

websocketServer :: WS.ServerApp
websocketServer pending = do
    sock <- WS.acceptRequest pending
    WS.sendTextData sock (encode defaultGameState)
    WS.sendClose sock ("" :: L.ByteString)

main = Warp.run 8000 (HWS.websocketsOr WS.defaultConnectionOptions websocketServer httpServer)
