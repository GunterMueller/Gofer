------------------------------------------------------------
-- The Dining Philosophers

main :: IO ()
main = philos 5

type Fork = MVar Bool

-- philosophers

philos :: Int -> IO ()
philos n = start $
  do win <- window [ title "Philosophers" ]
     can <- canvas [ height 300, width 300 ] win
     but <- button [ text "Quit", command quit ] win
     pack (can ^^ but)
     
     fs <- binds [ newMVar (i==n) | i <- [1..n] ]
     ps <- binds [ pHead xy can   | xy <- circle n ]

     forks [ phil p f | (p,f) <- zip ps (pairs fs) ]

pHead :: (Int,Int) -> Canvas -> GUI COval
pHead (x,y) = coval (x+130,y+130) (x+170,y+170) [fillColor "white"]

circle :: Int -> [(Int,Int)]
circle n = take n [ (r (sin x), r (cos x)) | x <- [0.0, step .. ] ]
 where
  step = (2.0*pi) / fromInteger n
  r x  = truncate (120.0 * x)

phil :: COval -> (Fork,Fork) -> GUI ()
phil pic (f1,f2) = loop $
  do b1 <- takeMVar f1
     if b1 then
       putMVar f1 b1
      else do
       b2 <- takeMVar f2
       eat True
       delay `apply` random 1200
       eat False
       putMVar f1 b2
       putMVar f2 b1
 where
  eat b = do cset pic (fillColor (if b then "black" else "white"))
             updateTask

-- auxiliary functions

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail (cycle xs))

loop :: GUI a -> GUI b
loop m = do m; updateTask; loop m

------------------------------------------------------------
-- the end.
