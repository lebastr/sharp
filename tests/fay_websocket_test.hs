import Prelude
import FFI
import FayFrp

import JQuery

data WebSocket = WebSocket

newWebSocket :: String -> Fay WebSocket
newWebSocket = ffi "createWebSocket(%1)"

sendToSocket :: WebSocket -> String -> Fay ()
sendToSocket = ffi "%1.send(%2)"

onMessage :: WebSocket -> (Event -> Fay ()) -> Fay ()
onMessage = ffi "onMessage (%1, %2)"

getData :: Event -> String
getData = ffi "%1.data"

createInputSource :: JQuery -> Fay (Source String)
createInputSource e = do
  source <- createFaySource e keyup
  return $ source $= (createSyncPipe $ return $ \_ -> getVal e)

createWebSocket :: String -> Fay (Source String, Sink String)
createWebSocket path = do
  socket <- newWebSocket path
  let sink = createSink $ \v -> sendToSocket socket v
  source <- createSource $ \c -> onMessage socket c
  let source' = source $= arr getData
  return (source', sink)

window :: String -> Sink String
window s = createSink $ \str -> do select s >>= setText str >> return ()

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  socket <- createWebSocket "/smartblobs.org"
  input <- select "#input1"
  inputSource <- createInputSource input
  let source = fst socket
      sink = snd socket
      w = window "#window1"
      app1 = inputSource $$ sink
      app2 = source $$ w
      app = app1 `parallel` app2

  runApp app