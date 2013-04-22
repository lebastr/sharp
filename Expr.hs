{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, ExistentialQuantification, ImpredicativeTypes, FlexibleContexts, UndecidableInstances #-}

module Expr where

import Link
import PrimitiveTypes
import Data.Monoid
import Control.Arrow
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Map
import Prelude hiding (lookup)
import Haste hiding (eval)
import Control.Monad.State.Strict


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
  SourceExpr :: Int -> Source a -> Expr (Source a)
  SourceSPipe :: Int -> Expr (Source a) -> Expr (SyncPipe a b) -> Expr (Source b)
  SourceAPipe :: Int -> Expr (Source a) -> Expr (AsyncPipe a b) -> Expr (Source b)
  MergeSource :: Int -> Expr (Source a) -> Expr (Source a) -> Expr (Source a)
  FilterSource :: Int -> (a -> Bool) -> Expr (Source a) -> Expr (Source a)

-- Sync Pipe section
  SPipeExpr :: SyncPipe a b -> Expr (SyncPipe a b)
  SPipeExprIO :: IO (SyncPipe a b) -> Expr (SyncPipe a b)
  SSPipe :: Expr (SyncPipe a b) -> Expr (SyncPipe b c) -> Expr (SyncPipe a c)
  FirstArrowS :: Expr (SyncPipe a b) -> Expr (SyncPipe (a,t) (b,t))
--  Accum :: (a -> t -> a) -> a -> Expr (SyncPipe t a)
  IfThenElseSync :: (a -> Bool) -> Expr (SyncPipe a b) -> Expr (SyncPipe a b) -> Expr (SyncPipe a b)
  Snapshot :: Expr (Source b) -> b -> Expr (SyncPipe a b)

-- Async Pipe section
  APipeExprIO :: IO (AsyncPipe a b) -> Expr (AsyncPipe a b)
  AAPipe :: Expr (AsyncPipe a b) -> Expr (AsyncPipe b c) -> Expr (AsyncPipe a c)
  FirstArrowA :: Expr (AsyncPipe a b) -> Expr (AsyncPipe (a,t) (b,t))
  ToAsync :: Expr (SyncPipe a b) -> Expr (AsyncPipe a b)
  IfThenElseAsync :: (a -> Bool) -> Expr (AsyncPipe a b) -> Expr (AsyncPipe a b) -> Expr (AsyncPipe a b)
  

class Draw a where
  draw :: a -> String

instance Draw String where
  draw s = "(" ++ show s ++ ")"

instance Draw Int where
  draw s = "(" ++ show s ++ ")"

instance (Draw a, Draw b) => Draw (a,b) where  
  draw (a,b) = "(" ++ draw a ++ " " ++ draw b ++ ")"
  
instance (Draw a, Draw b, Draw c) => Draw (a,b,c) where  
  draw (a,b,c) = "(" ++ draw a ++ " " ++ draw b ++ " " ++ draw c ++ ")"

instance (Draw a, Draw b, Draw c, Draw d) => Draw (a,b,c,d) where  
  draw (a,b,c,d) = "(" ++ draw a ++ " " ++ draw b ++ " " ++ draw c ++ " " ++ draw d ++ ")"

instance Draw (Expr a) where
  draw (Parallel app1 app2) = draw ("||", app1, app2)
  draw (SourceSink source sink) = draw (">-->", source, sink)

  draw (SinkExpr _) = "sink"
  draw (SPipeSink pipe sink) = draw ("-->", pipe, sink)
  draw (APipeSink pipe sink) = draw ("-->", pipe, sink)
  draw (MergeSink s1 s2) = draw ("+", s1, s2)

  draw (SourceExpr i s) = draw (i, "source")
  draw (SourceSPipe i s p) = draw (i, (">--", s, p))
  draw (SourceAPipe i s p) = draw (i, (">--", s, p))
  draw (MergeSource i s1 s2) = draw (i, ("+", s1, s2))
  draw (FilterSource i p s) = draw (i, ("filter", s))

  draw (SPipeExpr _) = "pipe"
  draw (SPipeExprIO _) = "pipeIO"
  draw (SSPipe p1 p2) = draw (">>>", p1, p2)
  draw (FirstArrowS p) = draw ("first", p)
  draw (IfThenElseSync _ p1 p2) = draw ("ifThenElse", p1, p2)
  draw (Snapshot s _) = draw ("snapshot", s)

  draw (APipeExprIO _) = "apipeIO"
  draw (AAPipe p1 p2) = draw (">>>", p1, p2)
  draw (FirstArrowA p) = draw ("first", p)
  draw (ToAsync p) = draw ("toAsync", p)
  draw (IfThenElseAsync _ p1 p2) = draw ("ifThenElse", p1, p2)

reify = unsafePerformIO $ newIORef (0 :: Int)

refId = modifyIORef reify (+1) >> readIORef reify

fromEvalM :: EvalM a -> a
fromEvalM = undefined

memoize :: Int -> EvalM (Source a) -> EvalM (Source a)
memoize p action = do
  m <- get
  case lookup p m (fromEvalM action) of
    Nothing -> do
      s <- action
      modify (insert p s)
      return s
    Just value -> do
      return value

type EvalM a = StateT Map (IO) a

class Eval a where
  type E a :: *
  eval :: a -> EvalM (E a)
  
instance Eval (Expr App) where
  type E (Expr App) = ()
  eval (Parallel app1 app2) = eval app1 >> eval app2
  eval (SourceSink source sink) = do
    a <- eval source 
    b <- eval sink
    lift $ a `link` b
    
instance Eval (Expr (Source a)) where
  type E (Expr (Source a)) = Source a
  eval (SourceExpr rId s) = memoize rId $ return s

  eval (SourceSPipe rId sExpr pExpr) = memoize rId $ do
    s <- eval sExpr
    p <- eval pExpr
    lift $ s `link` p
  
  eval (SourceAPipe rId sExpr pExpr) = memoize rId $ do
    s <- eval sExpr
    p <- eval pExpr
    lift $ s `link` p
  
  eval (MergeSource rId sExpr1 sExpr2) = memoize rId $ do
    s1 <- eval sExpr1
    s2 <- eval sExpr2
    return $ s1 `Merge` s2
    
  eval (FilterSource rId p sExpr) = memoize rId $ do
    s <- eval sExpr
    lift $ filterSource p s

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
    lift $ p `link` s
    
  eval (MergeSink sExpr1 sExpr2) = do
    s1 <- eval sExpr1
    s2 <- eval sExpr2
    return $ s1 `mappend` s2

instance Eval (Expr (SyncPipe a b)) where
  type E (Expr (SyncPipe a b)) = SyncPipe a b
  eval (SPipeExpr p) = return p
  eval (SPipeExprIO p) = lift p
  eval (SSPipe pExpr1 pExpr2) = do
    p1 <- eval pExpr1
    p2 <- eval pExpr2
    return $ p1 `link` p2

  eval (FirstArrowS pExpr) = do
    p <- eval pExpr
    return $ first p

  -- eval (Accum g v0) = do
  --   ref <- lift $ newIORef v0
  --   return $ SyncPipe $ \x -> do
  --     v <- readIORef ref
  --     writeIORef ref (g v x)
  --     return v

  eval (Snapshot sExpr v0) = do
    ref <- lift $ newIORef v0
    source <- eval sExpr
    lift $ source `link` (Sink $ writeIORef ref)
    return $ SyncPipe $ \_ -> readIORef ref

  eval (IfThenElseSync p pExpr1 pExpr2) = do
    pipe1 <- eval pExpr1
    pipe2 <- eval pExpr2
    return $ SyncPipe $ \v -> case p v of
      True -> runSyncPipe pipe1 v
      False -> runSyncPipe pipe2 v

instance Eval (Expr (AsyncPipe a b)) where
  type E (Expr (AsyncPipe a b)) = AsyncPipe a b
  eval (APipeExprIO p) = lift p
  eval (AAPipe pExpr1 pExpr2) = do
    p1 <- eval pExpr1
    p2 <- eval pExpr2
    lift $ p1 `link` p2
    
  eval (FirstArrowA pExpr) = do
    pipe <- eval pExpr
    case pipe of 
      FromSync p -> return $ FromSync $ first p
      AsyncPipe sink source -> do
        ref <- lift $ newIORef undefined
        let sink' = Sink $ \(v,t) -> do
              writeIORef ref t
              runSink sink v
            
            pipe' = SyncPipe $ \v -> do
              t <- readIORef ref
              return (v,t)
              
        source' <- lift $ (link :: Source a -> SyncPipe a b -> IO (Source b)) source pipe'
        return $ AsyncPipe sink' source'
    
  eval (ToAsync pExpr) = do
    p <- eval pExpr
    return $ FromSync p
    
  eval (IfThenElseAsync p pExpr1 pExpr2) = do
    pipe1 <- eval pExpr1
    pipe2 <- eval pExpr2
    ref1 <- lift $ newIORef []
    ref2 <- lift $ newIORef []
    let sink = Sink $ \v -> case p v of
          True -> do
            subs <- readIORef ref1
            forM_ subs $ \c -> runSink c v          
          False -> do
            subs <- readIORef ref2
            forM_ subs $ \c -> runSink c v
            
        source1 = Source ref1
        source2 = Source ref2
    
    source1' <- lift $ source1 `link` pipe1
    source2' <- lift $ source2 `link` pipe2
    
    return $ AsyncPipe sink $ Merge source1' source2'