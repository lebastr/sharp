import Prelude
import FFI
import FayFrp

import JQuery

type Point = (Double, Double)

data MouseEvent = Down Point
                | Up Point
                | Move Point

fromJust :: Maybe a -> a
fromJust (Just x) = x

mousedown' a b = mousedown a b >> return ()
mouseup' a b = mouseup a b >> return ()
mousemove' a b = mousemove a b >> return ()

createMouseSource :: JQuery -> Fay (Source MouseEvent)
createMouseSource element = do
  down <- createFaySource element mousedown'
  up <- createFaySource element mouseup'
  move <- createFaySource element mousemove'

  let posX = arr $ flip eventX element
      posY = arr $ flip eventY element
      posP = posX &&& posY
      
      down' = down $= posP $= arr Down
      up' = up $= posP $= arr Up
      move' = move $= posP $= arr Move

  return $ down' `mergeSource` up' `mergeSource` move'

window :: String -> Sink String
window s = createSink $ \str -> do select s >>= setText str >> return ()

moveTo :: Canvas -> Double -> Double -> Fay ()
moveTo = ffi "callMoveTo (%1, %2, %3)"

lineTo  :: Canvas -> Double -> Double -> Fay ()
lineTo = ffi "callLineTo (%1, %2, %3)"

beginPath  :: Canvas -> Fay ()
beginPath = ffi "callBeginPath(%1)"

closePath :: Canvas -> Fay ()
closePath = ffi "callClosePath (%1)"

stroke :: Canvas -> Fay ()
stroke = ffi "callStroke(%1)"

type Canvas = JQuery

data DrawCommand = Start Point
                 | Draw Point
                 | Stop
                 deriving (Show)

drawCanvas :: Canvas -> Sink DrawCommand
drawCanvas canvas = createSink $ \e -> case e of
  Start p -> do
    moveTo canvas (fst p) (snd p)
    beginPath canvas
  Draw p -> lineTo canvas (fst p) (snd p)
  Stop -> do
    closePath canvas
    stroke canvas

transform :: Source MouseEvent -> Source DrawCommand
transform ev = let pipe = accum (\s v -> case s of
                                    True -> case v of
                                      Move p -> (True, Just (Draw p))
                                      Up p -> (False, Just Stop)
                                    False -> case v of
                                      Down p -> (True, Just (Start p))
                                      Move p -> (False, Nothing)) False
                   f = filterSource (\v -> case v of
                                        Nothing -> False
                                        Just _ -> True)
               in f (ev $= pipe) $= arr fromJust

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  canvas <- select "#canvas"
  source <- createMouseSource canvas
  let w = window "#window1"
      ev = transform source $= trace "drawCmd"
      app1 = transform source $= trace "drawCmd" $$ drawCanvas canvas
      app2 = ev $= arr (\p -> case p of
                           Draw (x,y) -> show x ++ " " ++ show y
                           Start _ -> "start"
                           Stop  -> "stop") $$ w
  runApp $ app1 `parallel` app2