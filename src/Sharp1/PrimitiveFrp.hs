module PrimitiveFrp where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Cont
import Data.IORef
import Control.Monad.STM
import Control.Concurrent.STM.TVar

--------------------Pipe type--------------------

data Pipe a b = Sync (Kleisli (IO) a b)
              | Async (Kleisli (ContT () (IO)) a b)

toAsync :: Pipe a b -> Pipe a b
toAsync p@(Async _) = p
toAsync (Sync (Kleisli e)) = Async $ Kleisli $ lift . e

instance Category Pipe where
  id = Sync id
  (.) (Sync p0) (Sync p1) = Sync $ p0 . p1
  (.) (Async p0) (Async p1) = Async $ p0 . p1
  (.) p0@(Sync _) p1@(Async _) = toAsync p0 . p1
  (.) p0@(Async _) p1@(Sync _) = p0 . toAsync p1

instance Arrow Pipe where
  arr f = Sync $ arr f
  first (Sync a) = Sync $ first a
  first (Async a) = Async $ first a

----------------------- Source type -------------------

type Source a = ContT () (IO) a

clojureSources :: [Source a] -> IO (Source a)
clojureSources sources = do
  ref <- newIORef []
  forM_ sources $ \source -> do
    runContT source $ \v -> do
      subs <- readIORef ref
      forM_ subs $ \s -> s v

  return $ ContT $ \cps -> modifyIORef ref (++[cps])

sourcePipe :: Source a -> Pipe a b -> IO (Source b)
sourcePipe s p@(Sync _) = sourcePipe s $ toAsync p
sourcePipe s (Async (Kleisli p)) = clojureSources $ [s >>= p]

mergeSource :: Source a -> Source a -> IO (Source a)
mergeSource s1 s2 = clojureSources [s1,s2]

snapshot :: Source a -> a -> IO (Pipe t a)
snapshot source value = do
  ref <- newTVarIO value
  runContT source $ \v -> atomically $ writeTVar ref v
  return $ Sync $ Kleisli $ const $ atomically $ readTVar ref

createSource :: IO (Sink a, Source a)
createSource = do
  ref <- newIORef []
  let sink = \v -> do
        subs <- readIORef ref
        forM_ subs $ \s -> s v

      source = ContT $ \cps -> modifyIORef ref (++[cps])
  return (sink, source)

-------------------------- Sink type ---------------------------

type Sink a = a -> IO ()

sourceSink :: Source a -> Sink a -> IO ()
sourceSink source sink = runContT source sink

mergeSink :: Sink a -> Sink a -> Sink a
mergeSink s1 s2 = \s -> s1 s >> s2 s

pipeSink :: Pipe a b -> Sink b -> Sink a
pipeSink (Sync (Kleisli p)) s = \v -> p v >>= s
pipeSink (Async (Kleisli p)) s = \v -> runContT (p v) s
