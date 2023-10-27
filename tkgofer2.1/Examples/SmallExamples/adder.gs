-- simple application, 
-- no `do-notation' used
-- start with adder


adder :: IO ()
adder = start (
  window [title "Counter"]                      `bind` \w ->
  entry  [initValue 0] w                        `bind` \e ->
  button [text "Increment", command (incr e)] w `bind` \b ->
  pack (e ^-^ b)
  )
 
incr :: Entry Int -> GUI ()
incr e = getValue e `bind` (\x -> setValue e (x+1))
