{-# LANGUAGE GADTs #-}

module Sharp.PrimitiveFrp where

import Sharp.Expr
import Sharp.PrimitiveTypes
import Sharp.Platform
import Prelude

data Pipe a b = Pipe (Expr (PPipe a b))

data Source a = Source (Expr (PSource a))

data Sink a = Sink (Expr (PSink a))

data App = App (Expr EApp)

type Unique = JSRef Int

tag :: Unique -> Int
tag ref = unsafePerformJS $ modifyJSRef ref (+1) >> readJSRef ref

unique :: Unique
unique = unsafePerformJS $ newJSRef 0

filterSource :: (a -> Bool) -> Source a -> Source a
filterSource p (Source sexpr) = Source $ FilterSource (tag unique) p sexpr

createSink :: (a -> JS ()) -> Sink a
createSink s = Sink $ SinkExpr $ PSink s

createSource :: ((a -> JS ()) -> JS ()) -> JS (Source a)
createSource g = do
  s <- createPSource g
  return $ Source $ SourceExpr (tag unique) s


createSyncPipe :: JS (a -> JS b) -> Pipe a b
createSyncPipe action = Pipe $ PipeExpr $ do
  p <- action
  return $ Sync p

createAsyncPipe :: (a -> (b -> JS ()) -> JS ()) -> Pipe a b
createAsyncPipe action = Pipe $ PipeExpr $ createAsyncPPipe action

scanP :: (a -> t -> a) -> a -> Pipe t a
scanP f = accum g where
  g s v = let v' = f s v in (v',v')

accum :: (s -> a -> (s,b)) -> s -> Pipe a b
accum f state = createSyncPipe $ do
  acc <- newJSRef state
  return $ \v -> do
    s <- readJSRef acc
    let r = f s v
        s' = fst r
        v' = snd r
    writeJSRef acc s'
    return v'

snapshot :: Source b -> b -> Pipe a b
snapshot (Source source) v0 = Pipe $ Snapshot source v0

infixr 0 $$
infixl 1 $=
infixr 2 =$
infixr 2 =$=

($=) :: Source a -> Pipe a b -> Source b
($=) = sourcePipe

(=$) :: Pipe a b -> Sink b -> Sink a
(=$) = pipeSink

($$) :: Source a -> Sink a -> App
($$) = sourceSink

(=$=) :: Pipe a b -> Pipe b c -> Pipe a c
(=$=) = pipePipe

ifThenElsePipe :: (a -> Bool) -> Pipe a b -> Pipe a b -> Pipe a b
ifThenElsePipe p (Pipe pexpr1) (Pipe pexpr2) = Pipe $ IfThenElse p pexpr1 pexpr2

runApp :: App -> JS ()
runApp (App e) = evalApp e

parallel :: App -> App -> App
parallel (App e1) (App e2) = App $ e1 `Parallel` e2

-- Arrow implementation for Pipe

infixr 1 >>>
infixr 3 ***
infixr 3 &&&

idPipe :: Pipe a a
idPipe = createSyncPipe $ return $ \x -> return x

arr :: (a -> b) -> Pipe a b
arr f = createSyncPipe $ return $ return . f

first :: Pipe a b -> Pipe (a,t) (b,t)
first (Pipe pexpr) = Pipe $ FirstArrow pexpr

second :: Pipe a b -> Pipe (t,a) (t,b)
second a = f >>> first a >>> f where
  f = arr $ \(x,y) -> (y,x)

(>>>) = (=$=)

(***) :: Pipe b c -> Pipe b' c' -> Pipe (b,b') (c,c')
(***) p1 p2 = first p1 >>> second p2

(&&&) :: Pipe b c -> Pipe b c' -> Pipe b (c,c')
(&&&) p1 p2 = arr (\x -> (x,x)) >>> (p1 *** p2)

-- Functor Pipe

fmapPipe :: (a -> b) -> Pipe t a -> Pipe t b
fmapPipe f p = p >>> arr f

-- Applicative Functor instance

purePipe :: a -> Pipe t a
purePipe = arr . const

appPipe :: Pipe t (a -> b) -> Pipe t a -> Pipe t b
appPipe fp vp = (fp &&& vp) >>> arr (\(f,v) -> f v)

-- Functor Source

fmapSource :: (a -> b) -> Source a -> Source b
fmapSource f s = sourcePipe s $ arr f

-- Monoid Source

emptySource :: Source a
emptySource = Source $ SourceExpr (tag unique) EmptyPSource

mergeSource :: Source a -> Source a -> Source a
mergeSource (Source s1) (Source s2) = Source $ MergeSource (tag unique) s1 s2

-- Monoid Sink

emptySink :: Sink a
emptySink = Sink $ SinkExpr $ emptyPSink

mergeSink :: Sink a -> Sink a -> Sink a
mergeSink (Sink s1) (Sink s2) = Sink $ s1 `MergeSink` s2

sourceSink :: Source a -> Sink a -> App
sourceSink (Source source) (Sink sink) = App $ source `SourceSink` sink

pipeSink :: Pipe a b -> Sink b -> Sink a
pipeSink (Pipe p) (Sink s) = Sink $ p `PipeSink` s

sourcePipe :: Source a -> Pipe a b -> Source b
sourcePipe (Source source) (Pipe pipe) = Source $ SourcePipe (tag unique) source pipe

pipePipe :: Pipe a b -> Pipe b c -> Pipe a c
pipePipe (Pipe p1) (Pipe p2) = Pipe $ p1 `PipePipe` p2
