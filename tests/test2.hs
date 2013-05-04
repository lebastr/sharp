import Prelude
import PrimitiveTypes

import FFI
import JQuery

document :: Document
document = ffi "document"

console :: String -> Fay ()
console = ffi "console.log(%1)"

main = documentReady `flip` document $ \e -> do
  console "hello world"
  source <- select "#input1" >>= \i -> createSource $ \act -> keypress (\e -> getText i >>= act) i
  let sink = Sink $ \str -> do select "#window1" >>= setText str >> return ()
--  select "#input1" >>= keypress (\e -> select "#window1" >>= setText "test" >> return ())
  sourceSink source sink
  console "bye world"
