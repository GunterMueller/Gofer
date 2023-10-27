-- clock.gs: demonstration of clocks

-- flash: a flashing label
-- ticks: several clocks decrementing the same value


-- example1 

flash :: IO ()
flash = start $ do
  x <- timer [initValue 1000]
  w <- window [] 
  b <- button [ text "Start/Stop"
              , command (swapClock x)
              ] w
  q <- button [ text "Quit"
              , command (quitAll x) 
              ] w
  l <- label  [ width 4 ] w
  s <- hscale [ text "Speed"
              , scaleRange (1,2000)
              , self (command . changeClock x) 
              ] w
  pack (s ^-^ l ^-^ (b << q))
  csets x [command (swapColor l)] 


 where

    swapColor l = do
      c <- cget l background
      cset l ( background $ 
        case c of "red"     -> "green"
                  otherwise -> "red" )

    swapClock x = do
      a <- cget x active
      cset x (active (not a))
    
    changeClock x s = do
      t <- getValue s
      setValue x t
    
    quitAll x = do
      cset x (active False)
      quit


-- -------------------------------------------------------------   

-- example2 

ticks :: Int -> IO ()
ticks t = start $ do
  time <- clipboard [initValue t]
  win  <- window []
  inp  <- input [text "time", initValue t] win
  but  <- button [ text "Start Clock"
                 , command $
                     void $ timer [ initValue 1000
                                  , self (command . decr time inp)
                                  , active True
                                  ]
                 ] win
  pack (but ^-^ inp)
 where
  decr time inp clk = do 
    x <- getValue time
    setValue inp x
    if x > 0 then setValue time (x-1) else cset clk (active False) 


