import Sharp.PrimitiveTypes

import Prelude

import JQuery
import FFI

document :: Document
document = ffi "document"

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  source <- select "#input1" >>= \i -> createPSource $ \act -> keypress (\e -> getVal i >>= \t -> putStrLn ("source: " ++ t) >> act t) i
  let sink = PSink $ \str -> do select "#window1" >>= setText str >> return ()
--  select "#input1" >>= keypress (\e -> select "#window1" >>= setText "test" >> return ())
  sourcePSink source sink
  putStrLn "bye world"
