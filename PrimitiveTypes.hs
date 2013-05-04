{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, GADTs #-}
{-# LANGUAGE CPP #-}

module PrimitiveTypes where

#ifdef FAY
import Prelude
import FayRef
#else
import Data.IORef
import Control.Monad
#endif

#ifdef FAY
type JS = Fay
type JSRef = FayRef
newJSRef = newFayRef
readJSRef = readFayRef
modifyJSRef = modifyFayRef
writeJSRef = writeFayRef
#else
type JS = IO
type JSRef = IORef
newJSRef = newIORef
readJSRef = readIORef
modifyJSRef = modifyIORef

#endif

data Source a = Source (JSRef [Sink a])
              | Merge (Source a) (Source a)
              | EmptySource

createSource :: ((a -> JS ()) -> JS ()) -> JS (Source a)
createSource call = do
  ref <- newJSRef []
  call $ \v -> do
    subs <- readJSRef ref
    forM_ subs $ \c -> runSink c v
  return $ Source ref

-- cAsyncPipe :: ((a -> JS ()) -> JS ()) -> JS (Source a)
-- cAsyncPipe call = do
--   ref <- newJSRef []
--   let sink = Sink $ \v -> call $ \v' ->

-- delay :: Int -> JS (AsyncPipe a a)
-- delay dt = do
--   ref <- newJSRef []
--   let sink = Sink $ \v -> do
--         subs <- readJSRef ref
--         setTimeout dt $ forM_ subs $ \c -> runSink c v
--       source = Source ref
--   return $ Async sink source

filterSource :: (a -> Bool) -> Source a -> JS (Source a)
filterSource p source = do
  ref <- newJSRef []
  let sink = Sink $ \v -> do
        subs <- readJSRef ref
        case p v of
          False -> return ()
          True  -> forM_ subs $ \c -> runSink c v
  source `sourceSink` sink
  return $ Source ref

data Sink a = Sink { runSink :: a -> JS () }

data Pipe a b = Async (Sink a) (Source b)
              | Sync (a -> JS b)

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

emptySink :: Sink a
emptySink = Sink $ \_ -> return ()

mergeSink :: Sink a -> Sink a -> Sink a
mergeSink (Sink s1) (Sink s2) = Sink $ \v -> s1 v >> s2 v

sourceSink :: Source a -> Sink a -> JS ()
sourceSink (Source ref) sink = modifyJSRef ref (sink:)
sourceSink (Merge s1 s2) sink = s1 `sourceSink` sink >> s2 `sourceSink` sink
sourceSink EmptySource sink = return ()

sourcePipe :: Source a -> Pipe a b -> JS (Source b)
sourcePipe source@(Source ref) pipe = case pipe of
  Sync runPipe -> do
    ref1 <- newJSRef []
    let sink = Sink $ \v -> do
          subs <- readJSRef ref1
          case null subs of
            True -> return ()
            False -> do
              v' <- runPipe v
              forM_ subs $ \c -> runSink c v'

    modifyJSRef ref (sink:)
    return $ Source ref1

  Async sink source' -> do
    source `sourceSink` sink
    return source'

sourcePipe (Merge s1 s2) pipe = do
  s1' <- s1 `sourcePipe` pipe
  s2' <- s2 `sourcePipe` pipe
  return $ Merge s1' s2'

sourcePipe EmptySource pipe = return EmptySource

pipeSink :: Pipe a b -> Sink b -> JS (Sink a)
pipeSink (Sync p) (Sink s) = return $ Sink $ \v -> p v >>= s
pipeSink (Async sink source) sink1 = do
  source `sourceSink` sink1
  return sink

pipePipe :: Pipe a b -> Pipe b c -> JS (Pipe a c)
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

