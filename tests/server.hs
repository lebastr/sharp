-- websockets example
-- ==================

-- This is the Haskell implementation of the example for the WebSockets library. We
-- implement a simple multi-user chat program. A live demo of the example is
-- available [here](http://jaspervdj.be/websockets-example). In order to understand
-- this example, keep the [reference](http://jaspervdj.be/websockets/reference)
-- nearby to check out the functions we use.

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

main :: IO ()
main = do
  putStrLn "http://localhost:9160/client.html"
  Warp.runSettings Warp.defaultSettings
      { Warp.settingsPort = 9160
      , Warp.settingsIntercept = WaiWS.intercept application
      } staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

application :: WS.Request -> WS.WebSockets WS.Hybi00 ()
application rq = do
  WS.acceptRequest rq
  WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
  sink <- WS.getSink
  talk sink


talk sink = flip WS.catchWsError (\_ -> return ()) $ do
  msg <- WS.receiveData
  liftIO $ WS.sendSink sink (WS.textData (msg :: Text))
  talk sink
