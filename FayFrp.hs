module FayFrp ( module PrimitiveFrp
              , Time, document, trace, createFaySource
              , delay, setTimeout, tagTime ) where

import Prelude
import PrimitiveFrp

import FFI
import JQuery

document :: Document
document = ffi "document"

type Time = Int

trace :: (Show a) => String -> Pipe a a
trace tag = createSyncPipe $ return $ \v -> do
  putStrLn $ show v
  return v

createFaySource :: JQuery -> ((Event -> Fay ()) -> JQuery -> Fay ()) -> Fay (Source Event)
createFaySource e act = createSource (\c -> act c e)

delay :: Time -> Pipe a a
delay dt = createAsyncPipe $ \v c -> setTimeout dt (c v)

setTimeout :: Time -> Fay () -> Fay ()
setTimeout = ffi "window.setTimeout(%2, %1)"

tagTime :: Pipe a (Time, a)
tagTime = createSyncPipe $ return $ \v -> do
  t <- getTime
  return (t,v)

getTime :: Fay Time
getTime = getTimeObj >>= jsTime

data DateObj = DateObj

getTimeObj :: Fay DateObj
getTimeObj = ffi "new Date()"

jsTime :: DateObj -> Fay Time
jsTime = ffi "%1.getTime()"

