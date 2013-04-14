{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving #-}

module PrimitiveTypes where

import Control.Arrow
import Control.Category
import Control.Applicative
import Data.Monoid
import Prelude hiding (id, (.))
import Link

newtype Source a = Source { runSource :: (a -> IO ()) -> IO () }

newtype Sink a = Sink { runSink :: a -> IO () }

newtype SyncPipe a b = SyncPipe { runSyncPipe :: a -> IO b }

data AsyncPipe a b = AsyncPipe (Sink a) (Source b)
                   | FromSync (SyncPipe a b)

instance Category SyncPipe where
  id = SyncPipe $ runKleisli id
  (.) = flip link
  
instance Arrow SyncPipe where
  arr = SyncPipe . runKleisli . arr
  first = SyncPipe . runKleisli . first . Kleisli . runSyncPipe

instance Functor (SyncPipe t) where
  fmap f p = arr f . p
  
instance Applicative (SyncPipe t) where
  pure = arr . const
  (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

instance Monoid a => Monoid (SyncPipe t a) where
  mempty = arr $ const mempty
  mappend p1 p2 = mappend <$> p1 <*> p2
  
instance Functor (AsyncPipe t) where
  fmap g (FromSync p) = FromSync $ fmap g p
  fmap g (AsyncPipe sink source) = AsyncPipe sink $ fmap g source

instance Functor Source where
  fmap f s = s `link` (arr :: (a -> b) -> SyncPipe a b) f

instance Monoid (Source a) where
  mempty = Source $ \_ -> return ()
  mappend (Source s1) (Source s2) = Source $ \c -> s1 c >> s2 c
  
instance Monoid (Sink a) where
  mempty = Sink $ \_ -> return ()
  mappend (Sink s1) (Sink s2) = Sink $ \v -> s1 v >> s2 v
  
instance Link (Source a) (Sink a) where
  type C (Source a) (Sink a) = IO ()
  link (Source source) (Sink sink) = source sink

instance Link (SyncPipe a b) (Sink b) where
  type C (SyncPipe a b) (Sink b) = Sink a
  link (SyncPipe p) (Sink s) = Sink $ \v -> p v >>= s
  
instance Link (AsyncPipe a b) (Sink b) where
  type C (AsyncPipe a b) (Sink b) = IO (Sink a)
  link (AsyncPipe sink source) sink1 = do
    source `link` sink1
    return sink
  link (FromSync pipe) sink = return $ pipe `link` sink

instance Link (Source a) (SyncPipe a b) where
  type C (Source a) (SyncPipe a b) = Source b
  link (Source source) (SyncPipe pipe) = Source $ \c -> source $ \v -> pipe v >>= c

instance Link (Source a) (AsyncPipe a b) where
  type C (Source a) (AsyncPipe a b) = IO (Source b)
  link source (AsyncPipe sink source1) = do
    source `link` sink
    return source1

instance Link (SyncPipe a b) (SyncPipe b c) where
  type C (SyncPipe a b) (SyncPipe b c) = SyncPipe a c
  link (SyncPipe p1) (SyncPipe p2) = SyncPipe $ \v -> p1 v >>= p2

instance Link (AsyncPipe a b) (AsyncPipe b c) where
  type C (AsyncPipe a b) (AsyncPipe b c) = IO (AsyncPipe a c)
  link (FromSync p1) (FromSync p2) = return $ FromSync $ p1 `link` p2
  link (FromSync p1) (AsyncPipe sink source) = return $ AsyncPipe (p1 `link` sink) source
  link (AsyncPipe sink source) (FromSync p) = return $ AsyncPipe sink (source `link` p)
  link (AsyncPipe sink1 source1) (AsyncPipe sink2 source2) = do
    source1 `link` sink2
    return $ AsyncPipe sink1 source2
