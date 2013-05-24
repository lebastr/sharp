module PrimitiveFrp where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Cont
import Data.IORef
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Applicative
import Data.Maybe
import Control.Concurrent
import Control.Concurrent.MVar

type Time = Int

--------------------Pipe type--------------------

type Pipe a b = Kleisli (ContT () (IO)) a b

toAsync :: Pipe a b -> Pipe a b
toAsync (Kleisli pipe) = Kleisli $ \v -> ContT $ \cps -> do
  forkIO $ runContT (pipe v) cps
  return ()

toSync :: Pipe a b -> IO (Pipe a b)
toSync (Kleisli pipe) = do
  mvar <- newEmptyMVar
  return $ Kleisli $ \v -> do
    lift $ putMVar mvar ()
    v' <- pipe v
    lift $ takeMVar mvar
    return v'

delay :: Time -> Pipe a a
delay dt = Kleisli $ \v -> ContT $ \cps -> do
  forkIO $ threadDelay dt >> cps v
  return ()

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
sourcePipe s (Kleisli p) = clojureSources $ [s >>= p]

mergeSource :: Source a -> Source a -> IO (Source a)
mergeSource s1 s2 = clojureSources [s1,s2]

snapshot :: Source a -> a -> IO (Pipe t a)
snapshot source value = do
  ref <- newTVarIO value
  runContT source $ \v -> atomically $ writeTVar ref v
  return $ Kleisli $ const $ lift $ atomically $ readTVar ref

createSource :: IO (Sink a, Source a)
createSource = do
  ref <- newIORef []
  let sink = \v -> do
        subs <- readIORef ref
        forM_ subs $ \s -> s v

      source = ContT $ \cps -> modifyIORef ref (++[cps])
  return (sink, source)

filterSource :: (a -> Bool) -> Source a -> Source a
filterSource p s = ContT $ \cps -> runContT s $ \v -> case p v of
  False -> return ()
  True -> cps v

appSource :: Source (a -> b) -> Source a -> IO (Source b)
appSource fs vs = do
  fs' <- sourcePipe fs $ arr Just
  vs' <- sourcePipe vs $ arr Just
  fs'' <- sourcePipe fs $ arr $ \_ -> ()
  vs'' <- sourcePipe vs $ arr $ \_ -> ()
  pf <- snapshot fs' Nothing
  pv <- snapshot vs' Nothing
  source <- mergeSource fs'' vs''
  source' <- source `sourcePipe` ((pf &&& pv) >>> arr (\(f,v) -> f <*> v))
  (filterSource isJust source') `sourcePipe` (arr fromJust)

-------------------------- Sink type ---------------------------

type Sink a = a -> IO ()

sourceSink :: Source a -> Sink a -> IO ()
sourceSink source sink = runContT source sink

mergeSink :: Sink a -> Sink a -> Sink a
mergeSink s1 s2 = \s -> s1 s >> s2 s

pipeSink :: Pipe a b -> Sink b -> Sink a
pipeSink (Kleisli p) s = \v -> runContT (p v) s
