import Prelude
import PrimitiveTypes

import FFI
import JQuery

document :: Document
document = ffi "document"

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  source <- select "#input1" >>= \i -> createSource $ \act -> keypress (\e -> getVal i >>= \t -> putStrLn ("source: " ++ t) >> act t) i
  let sink = Sink $ \str -> do select "#window1" >>= setText str >> return ()
--  select "#input1" >>= keypress (\e -> select "#window1" >>= setText "test" >> return ())
  sourceSink source sink
  putStrLn "bye world"
 