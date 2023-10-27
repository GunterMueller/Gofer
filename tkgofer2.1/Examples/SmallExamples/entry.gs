
ex_entry :: IO ()
ex_entry = start $
  do w <- window []
     e <- entry [width 5] w
     cset e (on return $ do x <- getValue e; setValue e (x+1))
     pack e
