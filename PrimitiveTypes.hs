{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, GADTs #-}

module PrimitiveTypes where

-- import Prelude
import Data.IORef
import Control.Monad

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

-- delay :: Int -> IO (AsyncPipe a a)
-- delay dt = do
--   ref <- newIORef []
--   let sink = Sink $ \v -> do
--         subs <- readIORef ref
--         setTimeout dt $ forM_ subs $ \c -> runSink c v
--       source = Source ref
--   return $ Async sink source

filterSource :: (a -> Bool) -> Source a -> IO (Source a)
filterSource p source = do
  ref <- newIORef []
  let sink = Sink $ \v -> do
        subs <- readIORef ref
        case p v of
          False -> return ()
          True  -> forM_ subs $ \c -> runSink c v
  source `sourceSink` sink
  return $ Source ref

newtype Sink a = Sink { runSink :: a -> IO () }

data Pipe a b = Async (Sink a) (Source b)
              | Sync (a -> IO b)

-- instance Category SyncPipe where
--   id = SyncPipe $ runKleisli id
--   (.) = flip link

-- instance Arrow SyncPipe where
--   arr = SyncPipe . runKleisli . arr
--   first = SyncPipe . runKleisli . first . Kleisli . runSyncPipe

-- instance Functor (SyncPipe t) where
--   fmap f p = arr f . p

-- instance Applicative (SyncPipe t) where
--   pure = arr . const
--   (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

-- instance Monoid a => Monoid (SyncPipe t a) where
--   mempty = arr $ const mempty
--   mappend p1 p2 = mappend <$> p1 <*> p2

-- instance Monoid (Sink a) where
--   mempty = Sink $ \_ -> return ()
--   mappend (Sink s1) (Sink s2) = Sink $ \v -> s1 v >> s2 v

sourceSink :: Source a -> Sink a -> IO ()
sourceSink (Source ref) sink = modifyIORef ref (sink:)
sourceSink (Merge s1 s2) sink = s1 `sourceSink` sink >> s2 `sourceSink` sink
sourceSink EmptySource sink = return ()

sourcePipe :: Source a -> Pipe a b -> IO (Source b)
sourcePipe source@(Source ref) pipe = case pipe of
  Sync runPipe -> do
    ref1 <- newIORef []
    let sink = Sink $ \v -> do
          subs <- readIORef ref1
          case null subs of
            True -> return ()
            False -> do
              v' <- runPipe v
              forM_ subs $ \c -> runSink c v'

    modifyIORef ref (sink:)
    return $ Source ref1

  Async sink source' -> do
    source `sourceSink` sink
    return source'

sourcePipe (Merge s1 s2) pipe = do
  s1' <- s1 `sourcePipe` pipe
  s2' <- s2 `sourcePipe` pipe
  return $ Merge s1' s2'

sourcePipe EmptySource pipe = return EmptySource

pipeSink :: Pipe a b -> Sink b -> IO (Sink a)
pipeSink (Sync p) (Sink s) = return $ Sink $ \v -> p v >>= s
pipeSink (Async sink source) sink1 = do
  source `sourceSink` sink1
  return sink

pipePipe :: Pipe a b -> Pipe b c -> IO (Pipe a c)
pipePipe (Sync p1) (Sync p2) = return $ Sync $ \v -> p1 v >>= p2
pipePipe p1@(Sync _) (Async sink source) = do
  sink' <- p1 `pipeSink` sink
  return $ Async sink' source

pipePipe (Async sink source) pipe@(Sync _) = do
  s' <- source `sourcePipe` pipe
  return $ Async sink s'

pipePipe (Async sink1 source1) (Async sink2 source2) = do
    source1 `sourceSink` sink2
    return $ Async sink1 source2

