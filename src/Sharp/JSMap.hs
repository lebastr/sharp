{-# LANGUAGE RankNTypes, ImpredicativeTypes, ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Sharp.JSMap(JSMap, empty, keys, insert, lookup) where

import Sharp.Platform

import Prelude hiding(lookup)
#ifndef FAY
import qualified Data.Map as M
#endif

#ifdef FAY
type JSMap = [(Int,Any)]
#else
type JSMap = M.Map Int Any
#endif

data Any = forall a. Any a

empty :: JSMap
#ifdef FAY
empty = []
#else
empty = M.empty
#endif

insert :: Int -> a -> JSMap -> JSMap
insert i value m = m' where
#ifdef FAY
  m' = (i, Any value) : m
#else
  m' = M.insert i (Any value) m
#endif

keys :: JSMap -> [Int]
#ifdef FAY
keys = map fst
#else
keys = M.keys
#endif

lookup :: Int -> JSMap -> a -> Maybe a
lookup i m undef = case lookupX i m of
  Nothing -> Nothing
  Just (Any v) -> Just $ unsafeCoerce v

-- FIXME: ugly workaround. Fay doesn't support qualified names
#ifdef FAY
lookupX k [] = Nothing
lookupX k ((k1,v):xs) | k == k1 = Just v
                      | True = lookupX k xs
#else
lookupX = M.lookup
#endif