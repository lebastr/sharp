{-# LANGUAGE GADTs #-}

module Expr where
import Platform

import PrimitiveTypes
import JSMap

import Prelude hiding (lookup)

data App = App

data Expr a where
-- App section
  Parallel :: Expr App -> Expr App -> Expr App
  SourceSink :: Expr (Source a) -> Expr (Sink a) -> Expr App

-- Sink section
  SinkExpr  :: Sink a -> Expr (Sink a)
  PipeSink :: Expr (Pipe a b) -> Expr (Sink b) -> Expr (Sink a)
  MergeSink :: Expr (Sink a) -> Expr (Sink a) -> Expr (Sink a)

-- Source section
  SourceExpr :: Int -> Source a -> Expr (Source a)
  SourcePipe :: Int -> Expr (Source a) -> Expr (Pipe a b) -> Expr (Source b)
  MergeSource :: Int -> Expr (Source a) -> Expr (Source a) -> Expr (Source a)
  FilterSource :: Int -> (a -> Bool) -> Expr (Source a) -> Expr (Source a)

-- Pipe section
  PipeExpr :: JS (Pipe a b) -> Expr (Pipe a b)
  PipePipe :: Expr (Pipe a b) -> Expr (Pipe b c) -> Expr (Pipe a c)
  FirstArrow :: Expr (Pipe a b) -> Expr (Pipe (a,t) (b,t))
  IfThenElse :: (a -> Bool) -> Expr (Pipe a b) -> Expr (Pipe a b) -> Expr (Pipe a b)
  Snapshot :: Expr (Source b) -> b -> Expr (Pipe a b)

-- class Draw a where
--   draw :: a -> String

-- instance Draw String where
--   draw s = "(" ++ show s ++ ")"

-- instance Draw Int where
--   draw s = "(" ++ show s ++ ")"

-- instance (Draw a, Draw b) => Draw (a,b) where
--   draw (a,b) = "(" ++ draw a ++ " " ++ draw b ++ ")"

-- instance (Draw a, Draw b, Draw c) => Draw (a,b,c) where
--   draw (a,b,c) = "(" ++ draw a ++ " " ++ draw b ++ " " ++ draw c ++ ")"

-- instance (Draw a, Draw b, Draw c, Draw d) => Draw (a,b,c,d) where
--   draw (a,b,c,d) = "(" ++ draw a ++ " " ++ draw b ++ " " ++ draw c ++ " " ++ draw d ++ ")"

-- instance Draw (Expr a) where
--   draw (Parallel app1 app2) = draw ("||", app1, app2)
--   draw (SourceSink source sink) = draw (">-->", source, sink)

--   draw (SinkExpr _) = "sink"
--   draw (SPipeSink pipe sink) = draw ("-->", pipe, sink)
--   draw (APipeSink pipe sink) = draw ("-->", pipe, sink)
--   draw (MergeSink s1 s2) = draw ("+", s1, s2)

--   draw (SourceExpr i s) = draw (i, "source")
--   draw (SourceSPipe i s p) = draw (i, (">--", s, p))
--   draw (SourceAPipe i s p) = draw (i, (">--", s, p))
--   draw (MergeSource i s1 s2) = draw (i, ("+", s1, s2))
--   draw (FilterSource i p s) = draw (i, ("filter", s))

--   draw (SPipeExpr _) = "pipe"
--   draw (SPipeExprIO _) = "pipeIO"
--   draw (SSPipe p1 p2) = draw (">>>", p1, p2)
--   draw (FirstArrowS p) = draw ("first", p)
--   draw (IfThenElseSync _ p1 p2) = draw ("ifThenElse", p1, p2)
--   draw (Snapshot s _) = draw ("snapshot", s)

--   draw (APipeExprIO _) = "apipeIO"
--   draw (AAPipe p1 p2) = draw (">>>", p1, p2)
--   draw (FirstArrowA p) = draw ("first", p)
--   draw (ToAsync p) = draw ("toAsync", p)
--   draw (IfThenElseAsync _ p1 p2) = draw ("ifThenElse", p1, p2)

-- reify = unsafePerformIO $ newIORef (0 :: Int)

-- refId = modifyIORef reify (+1) >> readIORef reify

memo :: JSRef JSMap -> Int -> JS a -> JS a
memo ref p action = do
  m <- readJSRef ref
  case lookup p m undefined of
    Nothing -> do
      s <- action
      modifyJSRef ref (insert p s)
      return s
    Just value -> do
      return value

evalApp :: Expr App -> JS ()
evalApp (Parallel app1 app2) = evalApp app1 >> evalApp app2
evalApp (SourceSink source sink) = do
  state <- newJSRef empty
  a <- evalSource state source
  b <- evalSink state sink
  sourceSink a b

evalSource :: JSRef JSMap -> Expr (Source a) -> JS (Source a)
evalSource state (SourceExpr rId s) = memo state rId $ return s
evalSource state (SourcePipe rId sExpr pExpr) = memo state rId $ do
  s <- evalSource state sExpr
  p <- evalPipe state pExpr
  sourcePipe s p

evalSource state (MergeSource rId sExpr1 sExpr2) = memo state rId $ do
  s1 <- evalSource state sExpr1
  s2 <- evalSource state sExpr2
  return $ s1 `Merge` s2

evalSource state (FilterSource rId p sExpr) = memo state rId $ do
  s <- evalSource state sExpr
  filterSource p s

evalSink :: JSRef JSMap -> Expr (Sink a) -> JS (Sink a)
evalSink _ (SinkExpr s) = return s
evalSink state (PipeSink pExpr sExpr) = do
  p <- evalPipe state pExpr
  s <- evalSink state sExpr
  pipeSink p s

evalSink state (MergeSink sExpr1 sExpr2) = do
  s1 <- evalSink state sExpr1
  s2 <- evalSink state sExpr2
  return $ mergeSink s1 s2

evalPipe :: JSRef JSMap -> Expr (Pipe a b) -> JS (Pipe a b)
evalPipe _ (PipeExpr p) = p
evalPipe state (PipePipe pExpr1 pExpr2) = do
  p1 <- evalPipe state pExpr1
  p2 <- evalPipe state pExpr2
  pipePipe p1 p2

evalPipe state (Snapshot sExpr v0) = do
  ref <- newJSRef v0
  source <- evalSource state sExpr
  sourceSink source $ Sink $ writeJSRef ref
  return $ Sync $ \_ -> readJSRef ref

evalPipe state (FirstArrow pExpr) = do
  pipe <- evalPipe state pExpr
  case pipe of
    Sync p -> return $ Sync $ \(v,t) -> do
      v' <- p v
      return (v',t)

    Async sink source -> do
      ref <- newJSRef undefined
      let sink' = Sink $ \(v,t) -> do
            writeJSRef ref t
            runSink sink v

          pipe' = Sync $ \v -> do
            t <- readJSRef ref
            return (v,t)

      source' <- sourcePipe source pipe'
      return $ Async sink' source'

evalPipe state (IfThenElse p pExpr1 pExpr2) = do
  pipe1 <- evalPipe state pExpr1
  pipe2 <- evalPipe state pExpr2
  evalPipe' p pipe1 pipe2
  where
    evalPipe' p (Sync runPipe1) (Sync runPipe2) = do
      return $ Sync $ \v -> case p v of
        True -> runPipe1 v
        False -> runPipe2 v

    evalPipe' p pipe1 pipe2 = do
      ref1 <- newJSRef []
      ref2 <- newJSRef []
      let sink = Sink $ \v -> case p v of
            True -> do
              subs <- readJSRef ref1
              forM_ subs $ \c -> runSink c v
            False -> do
              subs <- readJSRef ref2
              forM_ subs $ \c -> runSink c v

          source1 = Source ref1
          source2 = Source ref2

      source1' <- sourcePipe source1 pipe1
      source2' <- sourcePipe source2 pipe2

      return $ Async sink $ Merge source1' source2'