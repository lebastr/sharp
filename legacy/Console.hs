module Console where

import Haste

foreign import ccall getTime :: IO Int
foreign import ccall callConsole :: JSString -> IO ()

printConsole :: (Show a) => a -> IO ()
printConsole = callConsole . toJSStr . show
