{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances #-}

module Expr where

import Link
import PrimitiveTypes
import Data.Monoid
import Control.Arrow
import Data.IORef

data App = App

data Expr a where
-- App section
  Parallel :: Expr App -> Expr App -> Expr App
  SourceSink :: Expr (Source a) -> Expr (Sink a) -> Expr App

-- Sink section
  SinkExpr  :: Sink a -> Expr (Sink a)
  SPipeSink :: Expr (SyncPipe a b) -> Expr (Sink b) -> Expr (Sink a)
  APipeSink :: Expr (AsyncPipe a b) -> Expr (Sink b) -> Expr (Sink a)
  MergeSink :: Expr (Sink a) -> Expr (Sink a) -> Expr (Sink a)

-- Source section
  SourceExpr :: Source a -> Expr (Source a)
  SourceSPipe :: Expr (Source a) -> Expr (SyncPipe a b) -> Expr (Source b)
  SourceAPipe :: Expr (Source a) -> Expr (AsyncPipe a b) -> Expr (Source b)
  MergeSource :: Expr (Source a) -> Expr (Source a) -> Expr (Source a)
  
-- Sync Pipe section
  SPipeExpr :: SyncPipe a b -> Expr (SyncPipe a b)
  SSPipe :: Expr (SyncPipe a b) -> Expr (SyncPipe b c) -> Expr (SyncPipe a c)
  FirstArrowS :: Expr (SyncPipe a b) -> Expr (SyncPipe (a,t) (b,t))
  
-- Async Pipe section
  APipeExpr :: AsyncPipe a b -> Expr (AsyncPipe a b)
  AAPipe :: Expr (AsyncPipe a b) -> Expr (AsyncPipe b c) -> Expr (AsyncPipe a c)
  FirstArrowA :: Expr (AsyncPipe a b) -> Expr (AsyncPipe (a,t) (b,t))
  ToAsync :: Expr (SyncPipe a b) -> Expr (AsyncPipe a b)
  
class Eval a where
  type E a :: *
  eval :: a -> IO (E a)
  
instance Eval (Expr App) where
  type E (Expr App) = ()
  eval (Parallel app1 app2) = eval app1 >> eval app2
  eval (SourceSink source sink) = do
    a <- eval source 
    b <- eval sink
    a `link` b
    
instance Eval (Expr (Source a)) where
  type E (Expr (Source a)) = Source a
  eval (SourceExpr s) = return s
  eval (SourceSPipe sExpr pExpr) = do
    s <- eval sExpr
    p <- eval pExpr
    return $ s `link` p
  
  eval (SourceAPipe sExpr pExpr) = do
    s <- eval sExpr
    p <- eval pExpr
    s `link` p
  
  eval (MergeSource sExpr1 sExpr2) = do
    s1 <- eval sExpr1
    s2 <- eval sExpr2
    return $ s1 `mappend` s2
  
instance Eval (Expr (Sink a)) where
  type E (Expr (Sink a)) = Sink a
  eval (SinkExpr s) = return s
  eval (SPipeSink pExpr sExpr) = do
    p <- eval pExpr
    s <- eval sExpr
    return $ p `link` s
    
  eval (APipeSink pExpr sExpr) = do
    p <- eval pExpr
    s <- eval sExpr
    p `link` s
    
  eval (MergeSink sExpr1 sExpr2) = do
    s1 <- eval sExpr1
    s2 <- eval sExpr2
    return $ s1 `mappend` s2

instance Eval (Expr (SyncPipe a b)) where
  type E (Expr (SyncPipe a b)) = SyncPipe a b
  eval (SPipeExpr p) = return p
  eval (SSPipe pExpr1 pExpr2) = do
    p1 <- eval pExpr1
    p2 <- eval pExpr2
    return $ p1 `link` p2

  eval (FirstArrowS pExpr) = do
    p <- eval pExpr
    return $ first p

instance Eval (Expr (AsyncPipe a b)) where
  type E (Expr (AsyncPipe a b)) = AsyncPipe a b
  eval (APipeExpr p) = return p
  eval (AAPipe pExpr1 pExpr2) = do
    p1 <- eval pExpr1
    p2 <- eval pExpr2
    p1 `link` p2
    
  eval (FirstArrowA pExpr) = do
    pipe <- eval pExpr
    case pipe of 
      FromSync p -> return $ FromSync $ first p
      AsyncPipe sink source -> do
        ref <- newIORef undefined
        let sink' = Sink $ \(v,t) -> do
              writeIORef ref t
              runSink sink v
            
            source' = Source $ \c -> runSource source $ \v -> do
              t <- readIORef ref
              c (v,t)
                
        return $ AsyncPipe sink' source'
    
  eval (ToAsync pExpr) = do
    p <- eval pExpr
    return $ FromSync p