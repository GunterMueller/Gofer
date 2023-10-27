-- Demonstration of `every' and `after'

main :: IO ()
main = start $
  do w  <- window []
     e1 <- entry [initValue 0] w
     e2 <- entry [initValue 0] w
     b1 <- button [text "Quit in 1 second!", command (after 1000 quit)] w
     every 1000 (updValue (+1) e1)
     every 100 (updValue (+1) e2)
     pack ((e1 << e2) ^-^ b1)
 
     b2 <- button [command $ (cset t (active False)), text "Stop"] w
     pack ((b1 << b2) ^-^ (e ^-^ e2))
