{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification #-}

module Map where

import qualified Data.Map as M
import Unsafe.Coerce


type Map = M.Map Int Any

data Any = forall a . Any a

empty :: Map 
empty = M.empty

insert :: Int -> a -> Map -> Map
insert i value m = m' where
  m' = M.insert i (Any value) m
  
keys = M.keys

lookup :: Int -> Map -> a -> Maybe a
lookup i m undef = case M.lookup i m of
  Nothing -> Nothing
  Just (Any v) -> Just $ unsafeCoerce v