module PolyFrp ( module Control.Arrow
               , module Control.Applicative
               , module Data.Monoid
               , Sink, Source, Pipe, filterSource
               , createSink, createSource, createSyncPipe, createAsyncPipe
               , scanP, accum, snapshot, ($=), (=$=), (=$)
               , ($$), ifThenElsePipe, parallel) where

import PrimitiveFrp hiding ((&&&), (***), arr)
import qualified PrimitiveFrp as F
import Data.Monoid
import Control.Arrow
import Control.Category
import Control.Applicative
import Prelude hiding ((.))

instance Category Pipe where
  id = idPipe
  (.) = flip pipePipe
  
instance Arrow Pipe where
  arr = F.arr
  first = F.first

instance Functor (Pipe t) where
  fmap f p = arr f . p
  
instance Applicative (Pipe t) where
  pure = arr . const
  (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

instance Monoid a => Monoid (Pipe t a) where
  mempty = arr $ const mempty
  mappend p1 p2 = mappend <$> p1 <*> p2
  
instance Functor Source where
  fmap = fmapSource

instance Monoid (Source a) where
  mempty = emptySource
  mappend = mergeSource
  
instance Monoid (Sink a) where
  mempty = emptySink
  mappend = mergeSink
