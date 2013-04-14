{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Link where

class Link a b where
  type C a b :: *
  link :: a -> b -> C a b
