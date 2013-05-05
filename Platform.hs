{-# LANGUAGE CPP #-}
module Platform(module Platform, forM_) where

#ifdef FAY
import Prelude
import FayRef
import FFI
#else
import Data.IORef
import Control.Monad
import qualified Unsafe.Coerce as UC
import qualified System.IO.Unsafe as SIU
#endif

#ifdef FAY
type JS = Fay
type JSRef = FayRef
newJSRef = newFayRef
readJSRef = readFayRef
modifyJSRef = modifyFayRef

writeJSRef = writeFayRef

unsafeCoerce :: a -> b
unsafeCoerce = ffi "%1"

unsafePerformJS :: JS a -> a
unsafePerformJS = ffi "%1"
#else
type JS = IO
type JSRef = IORef
newJSRef = newIORef
readJSRef = readIORef
modifyJSRef = modifyIORef
writeJSRef = writeIORef

unsafeCoerce = UC.unsafeCoerce
unsafePerformJS = SIU.unsafePerformIO
#endif
