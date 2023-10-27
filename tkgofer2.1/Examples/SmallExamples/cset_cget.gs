cset_cget =  start $
  do w <- window []
     b <- button [text "hello", self (command . rev)] w
     pack b
  where
    rev b = ctrans b text reverse 

ctrans b c f =
  do {x <- cget b c; cset b (c (f x))}
   


