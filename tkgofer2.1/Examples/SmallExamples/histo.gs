
ex_histo :: IO ()
ex_histo = start $ do
  w <- window [title "Histogram"]
  c <- canvas [width (xmax +10), height (ymax + 10)] w
  e <- entry  [self (on return . draw c)] w
  pack (c ^-^ e)

draw :: Canvas -> Entry String -> GUI ()
draw c e = do
  clearCanvas c
  v <- getValue e
  seqs [ (void . crect (x1,y1) (x2,y2) [fillColor "cyan"]) c
       | (x1,y1,x2,y2) <- (bars . map numval . words) v
       ] 

bars :: [Int] -> [(Int,Int,Int,Int)]
bars bs =
  let yunit  = fromInteger ymax / fromInteger (maximum bs)
      xunit  = xmax / length bs
      hght i = ymax + 5 - truncate (fromInteger i * yunit )
  in [(x+5, hght y, x+xunit+3, ymax) | (x,y) <- zip [0,xunit..] bs]

xmax = 150
ymax = 100
