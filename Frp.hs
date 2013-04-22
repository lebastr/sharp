{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, GADTs, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Frp where

import Control.Category
import Prelude hiding (id, (.))
import Control.Arrow
import Control.Applicative hiding (empty)
import Data.Monoid
import Link
import Expr hiding (App)
import qualified Expr as E
import qualified PrimitiveTypes as P
import System.IO.Unsafe (unsafePerformIO)
import Map
import Data.IORef
import Control.Monad.State.Strict

newtype AsyncPipe a b = AsyncPipe (Expr (P.AsyncPipe a b))

newtype SyncPipe a b = SyncPipe (Expr (P.SyncPipe a b))

newtype Source a = Source (Expr (P.Source a))

newtype Sink a = Sink (Expr (P.Sink a))

newtype App = App (Expr E.App)

filterSource :: (a -> Bool) -> Source a -> Source a
filterSource p (Source sexpr) = Source $ FilterSource (unsafePerformIO refId) p sexpr

createSink :: (a -> IO ()) -> Sink a
createSink s = Sink $ SinkExpr $ P.Sink s

createSource :: ((a -> IO ()) -> IO ()) -> IO (Source a)
createSource g = do
  s <- P.createSource g
  return $ Source $ SourceExpr (unsafePerformIO refId) s

createSyncPipe :: (a -> IO b) -> SyncPipe a b
createSyncPipe p = SyncPipe $ SPipeExpr $ P.SyncPipe p

createSyncPipeIO :: IO (a -> IO b) -> SyncPipe a b
createSyncPipeIO p = SyncPipe $ SPipeExprIO $ P.SyncPipe <$> p

-- createAsyncPipeIO :: IO (P.Sink a, P.Source a) -> AsyncPipe a b

scanP :: (a -> t -> a) -> a -> SyncPipe t a
scanP f v = createSyncPipeIO $ do
  acc <- newIORef v
  return $ \t -> do
    v0 <- readIORef acc
    let v1 = f v0 t 
    writeIORef acc v1
    return v1

accum :: (s -> a -> (s,b)) -> s -> SyncPipe a b
accum f state = createSyncPipeIO $ do
  acc <- newIORef state
  return $ \v -> do
    s <- readIORef acc
    let (s',v') = f s v
    writeIORef acc s'
    return v'


snapshot :: Source b -> b -> SyncPipe a b
snapshot (Source source) v0 = SyncPipe $ Snapshot source v0

delay dt = AsyncPipe $ APipeExprIO $ P.delay dt

-- createAsyncPipe :: 

-- createAsyncPipe :: (a -> IO ()) -> ((b -> IO ()) -> IO ()) -> AsyncPipe a b
-- createAsyncPipe sink source = AsyncPipe $ APipeExpr $ P.AsyncPipe (P.Sink sink) (P.Source source)

-- createAsyncPipeIO :: IO (Sink a, Source b) -> AsyncPipe a b
-- createAsyncPipeIO m = AsyncPipe $ IOAPipeExpr $ do
--   (sink, source) <- m                    
--   sink' <- evalStateT (eval sink) empty
--   sink' <- evalStateT (eval sink) empty
  
--   AsyncPipe $ IOAPipeExpr s

class Pipe p where
  (>--) :: Source a -> p a b -> Source b
  (-->) :: p a b -> Sink b -> Sink a
  ifThenElseP :: (a -> Bool) -> p a b -> p a b -> p a b

instance Pipe SyncPipe where
  (>--) = link
  (-->) = link
  ifThenElseP p (SyncPipe pexpr1) (SyncPipe pexpr2) = SyncPipe $ IfThenElseSync p pexpr1 pexpr2

instance Pipe AsyncPipe where
  (>--) = link
  (-->) = link
  ifThenElseP p (AsyncPipe pexpr1) (AsyncPipe pexpr2) = AsyncPipe $ IfThenElseAsync p pexpr1 pexpr2


(>-->) :: Source a -> Sink a -> App
(>-->) = link

runApp :: App -> IO ()
runApp (App e) = evalStateT (eval e) empty

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
  mempty = Source $ SourceExpr (unsafePerformIO refId) P.EmptySource
  mappend (Source s1) (Source s2) = Source $ MergeSource (unsafePerformIO refId) s1 s2
  
instance Monoid (Sink a) where
  mempty = Sink $ SinkExpr mempty
  mappend (Sink s1) (Sink s2) = Sink $ s1 `MergeSink` s2
  
instance Link (Source a) (Sink a) where
  type L (Source a) (Sink a) = App
  link (Source source) (Sink sink) = App $ source `SourceSink` sink

instance Link (SyncPipe a b) (Sink b) where
  type L (SyncPipe a b) (Sink b) = Sink a
  link (SyncPipe p) (Sink s) = Sink $ p `SPipeSink` s
  
instance Link (AsyncPipe a b) (Sink b) where
  type L (AsyncPipe a b) (Sink b) = Sink a
  link (AsyncPipe p) (Sink s) = Sink $ p `APipeSink` s

instance Link (Source a) (SyncPipe a b) where
  type L (Source a) (SyncPipe a b) = Source b
  link (Source source) (SyncPipe pipe) = Source $ SourceSPipe (unsafePerformIO refId) source pipe

instance Link (Source a) (AsyncPipe a b) where
  type L (Source a) (AsyncPipe a b) = Source b
  link (Source s) (AsyncPipe p) = Source $ SourceAPipe (unsafePerformIO refId) s p

instance Link (SyncPipe a b) (SyncPipe b c) where
  type L (SyncPipe a b) (SyncPipe b c) = SyncPipe a c
  link (SyncPipe p1) (SyncPipe p2) = SyncPipe $ p1 `SSPipe` p2

instance Link (AsyncPipe a b) (AsyncPipe b c) where
  type L (AsyncPipe a b) (AsyncPipe b c) = AsyncPipe a c
  link (AsyncPipe p1) (AsyncPipe p2) = AsyncPipe $ p1 `AAPipe` p2
