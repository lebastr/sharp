import Haste.DOM
import HasteFrp
import Prelude hiding ((.), id)

lastSeq :: Time -> Source a -> Source a
lastSeq dt s = let s' = s $= tagTime
                   pipe = id &&& (snapshot s' undefined)
                   ds = s' $= delay dt
                   ev = filterSource (\(a,b) -> fst a == fst b) $ (ds $= pipe)
               in (snd . snd) <$> ev

onlyChange :: (Eq a) => a -> Source a -> Source a
onlyChange v0 ev = let pipe = accum (\s v -> if s /= v
                                             then (v, (True, v))
                                             else (s, (False, v))) v0
                       ev' = filterSource fst (ev $= pipe)
                   in snd <$> ev'

isActive :: Time -> Source a -> Source Bool
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
      app1 = lastSeq 1000 i $$ w3
      app2 = i $$ w1
      app3 = (indicator <$> isActive 2000 i) $$ w0
      app4 = (i $= delay 1000) $$ w2
      app = app1 `parallel` app2 `parallel` app3 `parallel` app4
  runApp app 
