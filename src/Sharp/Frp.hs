{-# LANGUAGE CPP #-}
#ifdef FAY
module Sharp.Frp(module Sharp.FayFrp, module Prelude) where
import Sharp.FayFrp
import Prelude
#else
module Sharp.Frp(module Sharp.HasteFrp) where
import Sharp.HasteFrp
#endif
