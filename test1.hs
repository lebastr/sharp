import Haste
import Haste.Prim
import Haste.DOM
import Frp
import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.IORef
import Link
import Data.List
import Prelude hiding (id, (.))
import Control.Category
import Console
import Expr

trace :: (Show a) => String -> SyncPipe a a
trace text = createSyncPipe $ \v -> do
  writeLog $ text ++ " ---- " ++ show v
  return v

button :: Elem -> String -> IO (Source Int)
button parent name = do
  btn <- newElem "button"
  addChild btn parent
  setProp btn "textContent" name
  createSource $ \c -> do 
    setCallback btn OnClick c
    return ()

input :: Elem -> IO (Source String)
input parent = do
  i <- newElem "textarea"
  addChild i parent
  setStyle i "border-style" "solid"
  setStyle i "border-width" "2px"
  source <- createSource $ \c -> do
    setCallback i OnInput (c ())
    return ()
  let pipe = createSyncPipe $ \_ -> getProp i "value"
  return $ source >-- pipe
  
window :: (Show a) => Elem -> IO (Sink a)
window parent = do
  w <- newElem "div"
  addChild w parent
  return $ createSink $ \x -> do
    setProp w "textContent" (show x)

type Time = Int

data TObject a = TObject { call :: a -> IO ()
                         , linkCall :: (a -> IO ()) -> IO () }

newTObject :: IO (TObject a)
newTObject = do
  ref <- newIORef undefined
  return $ TObject { call = \v -> readIORef ref >>= mapM_ (\c -> c v)
                   , linkCall = \c -> modifyIORef ref (c:) }

-- delay :: Time -> AsyncPipe a a
-- delay dt = createAsyncPipeIO $ do
--   obj <- newTObject  
--   let sink = P.Sink $ \v -> setTimeout dt (call obj v)
--   source <- createSource $ \c -> linkCall obj c
--   return $ P.AsyncPipe sink source
  
tagTime :: SyncPipe a (Time, a)  
tagTime = createSyncPipe $ \v -> do
  t <- getTime
  return (t,v)
  
lastSeq :: Int -> Source a -> Source a  
lastSeq dt s = let s' = s >-- tagTime
                   pipe = (id &&& snapshot s' undefined)
                   ds = s' >-- delay dt
                   ev = filterSource (\(a,b) -> fst a == fst b) $ ds >-- pipe
               in (snd . snd) <$> ev
  
onlyChange :: (Eq a) => a -> Source a -> Source a
onlyChange v0 ev = let pipe = accum (\s v -> if s /= v 
                                             then (v, (True, v)) 
                                             else (s, (False, v))) v0
                       ev' = filterSource fst (ev >-- pipe)
                   in snd <$> ev'

isActive :: Int -> Source a -> Source Bool
isActive dt ev = let ev1 = lastSeq dt ev
                     f t = case t of
                       Left _ -> True
                       Right _ -> False
                     ev2 = f <$> (((Left <$> ev) `mappend` (Right <$> ev1)))
                 in onlyChange False ev2

main = do
  (Just d) <- elemById "div"
  w0  <- window d
  w1  <- window d
  w2  <- window d
  w3  <- window d
  i   <- input d 
  let indicator x | x = "You are writing..."
                  | otherwise = ""
      app1 = lastSeq 1000 i >--> w3
      app2 = i >--> w1
      app3 = (indicator <$> isActive 2000 i) >--> w0
      app4 = (i >-- delay 1000) >--> w2
      app = app1 `parallel` app2 `parallel` app3 `parallel` app4
  writeLog $ draw app
  runApp app 
