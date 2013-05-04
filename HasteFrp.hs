module HasteFrp ( module PolyFrp 
                , Time, delay, trace, button, window, input, tagTime) where

import PolyFrp
import Haste
import Haste.DOM
import Prelude hiding (id, (.))

type Time = Int

delay :: Time -> Pipe a a
delay dt = createAsyncPipe $ \v c -> setTimeout dt (c v)

trace :: (Show a) => String -> Pipe a a
trace text = createSyncPipe $ return $ \v -> do
  writeLog $ text ++ " ---- " ++ show v
  return v

button :: Elem -> String -> IO (Source Int)
button parent name = do
  btn <- newElem "button"
  addChild btn parent
  setProp btn "textContent" name
  createSource $ \c -> do
    setCallback btn OnClick c
    return ()

input :: Elem -> IO (Source String)
input parent = do
  i <- newElem "textarea"
  addChild i parent
  setStyle i "border-style" "solid"
  setStyle i "border-width" "2px"
  source <- createSource $ \c -> do
    setCallback i OnInput (c ())
    return ()
  let pipe = createSyncPipe $ return $ \_ -> getProp i "value"
  return $ source $= pipe

window :: (Show a) => Elem -> IO (Sink a)
window parent = do
  w <- newElem "div"
  addChild w parent
  return $ createSink $ \x -> do
    setProp w "textContent" (show x)

tagTime :: Pipe a (Time, a)
tagTime = createSyncPipe $ return $ \v -> do
  t <- getTime
  return (t,v)

