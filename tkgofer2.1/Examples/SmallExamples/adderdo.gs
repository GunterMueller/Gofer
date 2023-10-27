-- same as adder.gs
-- using do notation
-- start with: adder

adder :: IO ()
adder = start $
  do w <- window [title "Counter"]
     e <- entry  [initValue 0] w
     b <- button [text "Increment", command (incr e)] w
     pack (e ^-^ b)

incr :: Entry Int -> GUI ()
incr e = do x <- getValue e ; setValue e (x+1)
