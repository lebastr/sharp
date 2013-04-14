{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Frp where

import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative
import Data.Monoid
import Link
import Expr hiding (App)
import qualified Expr as E
import qualified PrimitiveTypes as P

newtype AsyncPipe a b = AsyncPipe (Expr (P.AsyncPipe a b))

newtype SyncPipe a b = SyncPipe (Expr (P.SyncPipe a b))

newtype Source a = Source (Expr (P.Source a))

newtype Sink a = Sink (Expr (P.Sink a))

newtype App = App (Expr E.App)

runApp :: App -> IO ()
runApp (App e) = eval e

parallel :: App -> App -> App
parallel (App e1) (App e2) = App $ e1 `Parallel` e2

toAsync :: SyncPipe a b -> AsyncPipe a b
toAsync (SyncPipe p) = AsyncPipe $ ToAsync p 

instance Category SyncPipe where
  id = SyncPipe $ SPipeExpr $ id
  (.) = flip link
  
instance Arrow SyncPipe where
  arr = SyncPipe . SPipeExpr . arr
  first (SyncPipe p) = SyncPipe $ FirstArrowS p

instance Functor (SyncPipe t) where
  fmap f p = arr f . p
  
instance Applicative (SyncPipe t) where
  pure = arr . const
  (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

instance Monoid a => Monoid (SyncPipe t a) where
  mempty = arr $ const mempty
  mappend p1 p2 = mappend <$> p1 <*> p2
  
instance Category AsyncPipe where
  id = toAsync id
  (.) = flip link
  
instance Arrow AsyncPipe where
  arr = toAsync . arr
  first (AsyncPipe p) = AsyncPipe $ FirstArrowA p

instance Functor (AsyncPipe t) where
  fmap f p = arr f . p
  
instance Applicative (AsyncPipe t) where
  pure = arr . const
  (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

instance Monoid a => Monoid (AsyncPipe t a) where
  mempty = arr $ const mempty
  mappend p1 p2 = mappend <$> p1 <*> p2

instance Functor Source where
  fmap f s = s `link` (arr :: (a -> b) -> SyncPipe a b) f

instance Monoid (Source a) where
  mempty = Source $ SourceExpr mempty
  mappend (Source s1) (Source s2) = Source $ s1 `MergeSource` s2
  
instance Monoid (Sink a) where
  mempty = Sink $ SinkExpr mempty
  mappend (Sink s1) (Sink s2) = Sink $ s1 `MergeSink` s2
  
instance Link (Source a) (Sink a) where
  type C (Source a) (Sink a) = App
  link (Source source) (Sink sink) = App $ source `SourceSink` sink

instance Link (SyncPipe a b) (Sink b) where
  type C (SyncPipe a b) (Sink b) = Sink a
  link (SyncPipe p) (Sink s) = Sink $ p `SPipeSink` s
  
instance Link (AsyncPipe a b) (Sink b) where
  type C (AsyncPipe a b) (Sink b) = Sink a
  link (AsyncPipe p) (Sink s) = Sink $ p `APipeSink` s

instance Link (Source a) (SyncPipe a b) where
  type C (Source a) (SyncPipe a b) = Source b
  link (Source source) (SyncPipe pipe) = Source $ source `SourceSPipe` pipe

instance Link (Source a) (AsyncPipe a b) where
  type C (Source a) (AsyncPipe a b) = Source b
  link (Source s) (AsyncPipe p) = Source $ s `SourceAPipe` p

instance Link (SyncPipe a b) (SyncPipe b c) where
  type C (SyncPipe a b) (SyncPipe b c) = SyncPipe a c
  link (SyncPipe p1) (SyncPipe p2) = SyncPipe $ p1 `SSPipe` p2

instance Link (AsyncPipe a b) (AsyncPipe b c) where
  type C (AsyncPipe a b) (AsyncPipe b c) = AsyncPipe a c
  link (AsyncPipe p1) (AsyncPipe p2) = AsyncPipe $ p1 `AAPipe` p2
