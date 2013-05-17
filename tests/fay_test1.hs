import Sharp.Frp

import JQuery

-- getTime :: Fay Time
-- getTime = ffi "var d = new Date(); var t = d.getTime(); return t;"

lastSeq :: Time -> Source a -> Source a
lastSeq dt s = let s' = s $= tagTime
                   pipe = idPipe &&& (snapshot s' undefined)
                   ds = s' $= delay dt
                   ev = filterSource (\(a,b) -> fst a == fst b) $ (ds $= pipe)
               in ev $= arr (snd . snd)

onlyChange :: (Eq a, Show a) => a -> Source a -> Source a
onlyChange v0 ev = let pipe = accum (\s v -> if s /= v
                                             then (v, (True, v))
                                             else (s, (False, v))) v0
                       ev' = filterSource fst (ev $= pipe)
                   in ev' $= arr snd

isActive :: (Show a) => Time -> Source a -> Source Bool
isActive dt ev = let ev1 = lastSeq dt ev
                     f t = case t of
                       Left _ -> True
                       Right _ -> False
                     ev2 = ((ev $= arr Left) `mergeSource` (ev1 $= arr Right)) $= arr f
                 in onlyChange False ev2

window :: String -> Sink String
window s = createSink $ \str -> do select s >>= setText str >> return ()

main = documentReady `flip` document $ \e -> do
  putStrLn "hello world"
  input <- select "#input1"
  source <- createFaySource input keyup
  let pipe = createSyncPipe $ return $ \e -> getVal input
      i = source $= pipe
      w1 = window "#window1"
      w2 = window "#window2"
      w3 = window "#window3"
      w4 = window "#window4"
      indicator x | x = "You are writing..."
                  | otherwise = ""

      app1 = (isActive 2000 i $= arr indicator) $$ w1
      app2 = i $$ w2
      app3 = (i $= delay 1000) $$ w3
      app4 = lastSeq 1000 i $$ w4
      app = app1 `parallel` app2 `parallel` app3 `parallel` app4

  runApp app
  putStrLn "bye world"
