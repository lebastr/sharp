{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable,  
    GADTs #-}

module PrimitiveTypes where

import Control.Arrow
import Control.Category
import Control.Applicative
import Data.Monoid
import Prelude hiding (id, (.))
import Link
import Data.Typeable
import Data.IORef
import Control.Monad
import Haste

data Source a = Source (IORef [Sink a])
              | Merge (Source a) (Source a)
              | EmptySource

createSource :: ((a -> IO ()) -> IO ()) -> IO (Source a)
createSource call = do
  ref <- newIORef []
  call $ \v -> do
    subs <- readIORef ref
    forM_ subs $ \c -> runSink c v
  return $ Source ref

-- cAsyncPipe :: ((a -> IO ()) -> IO ()) -> IO (Source a)
-- cAsyncPipe call = do
--   ref <- newIORef []
--   let sink = Sink $ \v -> call $ \v' -> 

delay :: Int -> IO (AsyncPipe a a)
delay dt = do
  ref <- newIORef []
  let sink = Sink $ \v -> do
        subs <- readIORef ref                    
        setTimeout dt $ forM_ subs $ \c -> runSink c v
      source = Source ref
  return $ AsyncPipe sink source
  
filterSource :: (a -> Bool) -> Source a -> IO (Source a)
filterSource p source = do
  ref <- newIORef []
  let sink = Sink $ \v -> do
        subs <- readIORef ref
        case p v of
          False -> return ()
          True  -> forM_ subs $ \c -> runSink c v
  source `link` sink
  return $ Source ref

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
  
instance Monoid (Sink a) where
  mempty = Sink $ \_ -> return ()
  mappend (Sink s1) (Sink s2) = Sink $ \v -> s1 v >> s2 v
  

instance Link (Source a) (Sink a) where
  type L (Source a) (Sink a) = IO ()
  link (Source ref) sink = modifyIORef ref (sink:)
  link (Merge s1 s2) sink = s1 `link` sink >> s2 `link` sink
  link EmptySource sink = return ()
  
instance Link (Source a) (SyncPipe a b) where
  type L (Source a) (SyncPipe a b) = IO (Source b)
  link (Source ref) pipe = do
    ref1 <- newIORef []
    let sink = Sink $ \v -> do
          subs <- readIORef ref1
          case null subs of
            True -> return ()
            False -> do
              v' <- runSyncPipe pipe v
              forM_ subs $ \c -> runSink c v'
    
    modifyIORef ref (sink:)
    return $ Source ref1
  
  link (Merge s1 s2) pipe = do
    s1' <- s1 `link` pipe 
    s2' <- s2 `link` pipe
    return $ Merge s1' s2'
  
  link EmptySource pipe = return EmptySource

instance Link (Source a) (AsyncPipe a b) where
  type L (Source a) (AsyncPipe a b) = IO (Source b)
  link source@(Source _) (FromSync pipe) = source `link` pipe
  link source@(Source _) (AsyncPipe sink source') = do
    source `link` sink
    return source'

  link (Merge s1 s2) pipe = do
    s1' <- s1 `link` pipe 
    s2' <- s2 `link` pipe
    return $ Merge s1' s2'
  
  link EmptySource pipe = return EmptySource
  
instance Link (SyncPipe a b) (Sink b) where
  type L (SyncPipe a b) (Sink b) = Sink a
  link (SyncPipe p) (Sink s) = Sink $ \v -> p v >>= s
  
instance Link (AsyncPipe a b) (Sink b) where
  type L (AsyncPipe a b) (Sink b) = IO (Sink a)
  link (AsyncPipe sink source) sink1 = do
    source `link` sink1
    return sink
  link (FromSync pipe) sink = return $ pipe `link` sink

instance Link (SyncPipe a b) (SyncPipe b c) where
  type L (SyncPipe a b) (SyncPipe b c) = SyncPipe a c
  link (SyncPipe p1) (SyncPipe p2) = SyncPipe $ \v -> p1 v >>= p2

instance Link (AsyncPipe a b) (AsyncPipe b c) where
  type L (AsyncPipe a b) (AsyncPipe b c) = IO (AsyncPipe a c)
  link (FromSync p1) (FromSync p2) = return $ FromSync $ p1 `link` p2
  link (FromSync p1) (AsyncPipe sink source) = return $ AsyncPipe (p1 `link` sink) source
  link (AsyncPipe sink source) (FromSync p) = do
    s' <- source `link` p                                          
    return $ AsyncPipe sink s'
  link (AsyncPipe sink1 source1) (AsyncPipe sink2 source2) = do
    source1 `link` sink2
    return $ AsyncPipe sink1 source2
