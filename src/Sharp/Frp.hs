{-# LANGUAGE CPP #-}
#ifdef FAY
module Sharp.Frp(module Sharp.FayFrp) where
import Sharp.FayFrp
#else
module Sharp.Frp(module Sharp.HasteFrp) where
import Sharp.HasteFrp
#endif
