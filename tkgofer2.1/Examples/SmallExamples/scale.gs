
ex_scale :: IO ()
ex_scale = start $
  do w <- window [title "small scale application"]
     s1 <- makeScale "speed (m/s)" w
     s2 <- makeScale "distance (m)" w
     l1 <- label [text "time (s) "] w
     l2 <- label [width 10, relief "ridge"] w
     setCommands s1 s2 l2
     pack ((s1 ^-^ s2) <|< (l1 <*-< l2))
  where
    makeScale lab win =
      hscale [ scaleRange (0, 99)
             , tickInterval 10
             , text lab
             , height 400
             ] win
    setCommands s1 s2 l2 =
      let slide = do v <- getValue s1
                     d <- getValue s2
                     cset l2 ((text . take 4 . time d) v)
          time d 0 = "0.0"
          time d v = show ((fromInteger d / fromInteger v) :: Float)
      in do cset s1 (command slide)
            cset s2 (command slide)
