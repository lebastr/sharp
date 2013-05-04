module HasteFrp ( module PolyFrp ) where

import PolyFrp
import Haste

type Time = Int

delay :: Time -> Pipe a a
delay dt = createAsyncPipe $ \v c -> setTimeout dt (c v)
