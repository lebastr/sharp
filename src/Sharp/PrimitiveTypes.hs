{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable, GADTs #-}
module Sharp.PrimitiveTypes where

import Prelude
import Sharp.Platform

data PSource a = PSource (JSRef [PSink a])
              | Merge (PSource a) (PSource a)
              | EmptyPSource

createPSource :: ((a -> JS ()) -> JS ()) -> JS (PSource a)
createPSource call = do
  ref <- newJSRef []
  call $ \v -> do
    subs <- readJSRef ref
    forM_ subs $ \c -> runPSink c v
  return $ PSource ref

createAsyncPPipe :: (a -> (b -> JS ()) -> JS ()) -> JS (PPipe a b)
createAsyncPPipe action = do
  ref <- newJSRef []
  let sink = PSink $ \v -> do
        subs <- readJSRef ref
        action v $ \v' -> forM_ subs $ \c -> runPSink c v'
      source = PSource ref
  return $ Async sink source

filterPSource :: (a -> Bool) -> PSource a -> JS (PSource a)
filterPSource p source = do
  ref <- newJSRef []
  let sink = PSink $ \v -> do
        subs <- readJSRef ref
        case p v of
          False -> return ()
          True  -> forM_ subs $ \c -> runPSink c v
  source `sourcePSink` sink
  return $ PSource ref

data PSink a = PSink { runPSink :: a -> JS () }

data PPipe a b = Async (PSink a) (PSource b)
               | Sync (a -> JS b)

-- instance Category SyncPPipe where
--   id = SyncPPipe $ runKleisli id
--   (.) = flip link

-- instance Arrow SyncPPipe where
--   arr = SyncPPipe . runKleisli . arr
--   first = SyncPPipe . runKleisli . first . Kleisli . runSyncPPipe

-- instance Functor (SyncPPipe t) where
--   fmap f p = arr f . p

-- instance Applicative (SyncPPipe t) where
--   pure = arr . const
--   (<*>) fp vp = arr (\(f,v) -> f v) . (fp &&& vp)

-- instance Monoid a => Monoid (SyncPPipe t a) where
--   mempty = arr $ const mempty
--   mappend p1 p2 = mappend <$> p1 <*> p2

-- instance Monoid (PSink a) where
--   mempty = PSink $ \_ -> return ()
--   mappend (PSink s1) (PSink s2) = PSink $ \v -> s1 v >> s2 v

emptyPSink :: PSink a
emptyPSink = PSink $ \_ -> return ()

mergePSink :: PSink a -> PSink a -> PSink a
mergePSink (PSink s1) (PSink s2) = PSink $ \v -> s1 v >> s2 v

sourcePSink :: PSource a -> PSink a -> JS ()
sourcePSink (PSource ref) sink = modifyJSRef ref (sink:)
sourcePSink (Merge s1 s2) sink = (s1 `sourcePSink` sink) >> (s2 `sourcePSink` sink)
sourcePSink EmptyPSource sink = return ()

sourcePPipe :: PSource a -> PPipe a b -> JS (PSource b)
sourcePPipe source@(PSource ref) pipe = case pipe of
  Sync runPPipe -> do
    ref1 <- newJSRef []
    let sink = PSink $ \v -> do
          subs <- readJSRef ref1
          case null subs of
            True -> return () -- TODO: think if it's a possible case
            False -> do
              v' <- runPPipe v
              forM_ subs $ \c -> runPSink c v'

    modifyJSRef ref (sink:)
    return $ PSource ref1

  Async sink source' -> do
    source `sourcePSink` sink
    return source'

sourcePPipe (Merge s1 s2) pipe = do
  s1' <- s1 `sourcePPipe` pipe
  s2' <- s2 `sourcePPipe` pipe
  return $ Merge s1' s2'

sourcePPipe EmptyPSource pipe = return EmptyPSource

pipePSink :: PPipe a b -> PSink b -> JS (PSink a)
pipePSink (Sync p) (PSink s) = return $ PSink $ \v -> p v >>= s
pipePSink (Async sink source) sink1 = do
  source `sourcePSink` sink1
  return sink

pipePPipe :: PPipe a b -> PPipe b c -> JS (PPipe a c)
pipePPipe (Sync p1) (Sync p2) = return $ Sync $ \v -> p1 v >>= p2
pipePPipe p1@(Sync _) (Async sink source) = do
  sink' <- p1 `pipePSink` sink
  return $ Async sink' source

pipePPipe (Async sink source) pipe@(Sync _) = do
  s' <- source `sourcePPipe` pipe
  return $ Async sink s'

pipePPipe (Async sink1 source1) (Async sink2 source2) = do
    source1 `sourcePSink` sink2
    return $ Async sink1 source2
