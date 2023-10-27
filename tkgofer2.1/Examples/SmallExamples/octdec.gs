

data Oct = Oct Int

instance GUIValue Oct where
  tk_defaultValue = Oct 0
  tk_convert s | all (flip elem "01234567") s = Tk_Ok (Oct (numval s))
  tk_convert s | otherwise = Tk_Err ("Invalid Oct String: " ++ s)
instance Text Oct where
  showsPrec d (Oct x) = shows x


ex_octdec = (start . openWindow [title "Convert"]) conv where
  conv w =
    do (f1,e1) <- input w "dec"
       (f2,e2) <- input w "oct"
       doconv (\n -> Oct (fromTo 10 8 n)) e1 e2
       doconv (\(Oct n) -> fromTo 8 10 n) e2 e1
       result (f1 << f2)

  input w s =
    do l <- label [text s] w
       e <- entry [width 9] w
       result ((l ^-^ e),e)

  doconv f a b =
    cset a (on return (do {x <- getValue a; setValue b (f x)}))

  fromTo n m = foldr (\a b -> b*n + a) 0 . digits m
    where digits j n = map (`mod` j) ((takeWhile (>0) . iterate (`div` j)) n)

