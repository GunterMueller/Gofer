
ex_entry_short :: IO ()
ex_entry_short = 
  (start . openWindow []) 
    (entry [self $ on return . updValue (1+), initValue 0])
