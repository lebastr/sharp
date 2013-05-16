{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Link where

class Link a b where
  type L a b :: *
  link :: a -> b -> L a b
