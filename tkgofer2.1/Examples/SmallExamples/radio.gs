
ex_radio :: IO ()
ex_radio = start $
  do w <- window []
     (ls1, r1) <- traffic w
     (ls2, r2) <- traffic w
     seqs (control ls1 r1 r2 ++ control ls2 r2 r1)
     pack (vertical ls1 << vertical ls2)
  where
    traffic w =
      do bs <- binds [radiobutton [indicatorColor c] w
                     | c <- ["red", "yellow", "green"]
                     ]
         r  <- radio [initValue 1] bs
         result (bs, r)

    control ls i j =
      [cset b (command $ do x <- getValue i; setValue j (2-x)) | b <- ls]
