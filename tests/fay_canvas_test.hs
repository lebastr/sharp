import Prelude
import FFI
import Sharp.FayFrp

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

setColor :: Canvas -> Fay ()
setColor = ffi "callColor(%1)"

closePath :: Canvas -> Fay ()
closePath = ffi "callClosePath (%1)"

stroke :: Canvas -> Fay ()
stroke = ffi "callStroke(%1)"

type Canvas = JQuery

drawCanvas :: Canvas -> Sink (Point,Point)
drawCanvas canvas = createSink $ \((x0,y0),(x1,y1)) -> do
    moveTo canvas x0 y0
--    beginPath canvas
    setColor canvas
    lineTo canvas x1 y1
--    closePath canvas
    stroke canvas

transform :: Source MouseEvent -> Source (Point,Point)
transform ev = let pipe = accum (\s v -> case s of
                                    Just p0 -> case v of
                                      Down p1  -> (Just p1, Just (p0,p1))
                                      Move p1 -> (Just p1, Just (p0,p1))
                                      Up p -> (Nothing, Nothing)
                                    Nothing -> case v of
                                      Down p -> (Just p, Nothing)
                                      Move p -> (Nothing, Nothing)
                                      Up p   -> (Nothing,Nothing)) Nothing
                   filter = filterSource (\s -> case s of
                                             Just _ -> True
                                             _ -> False)

               in filter (ev $= pipe) $= arr fromJust

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  canvas <- select "#canvas"
  source <- createMouseSource canvas
  let w = window "#window1"
      app1 = transform source $$ drawCanvas canvas
  runApp app1