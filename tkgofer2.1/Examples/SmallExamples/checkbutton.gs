
ex_checkbutton :: IO ()
ex_checkbutton = start $
  do w   <- window [title "Check this out!"]
     l1  <- label [text "The moon is made of cheese"] w
     cb1 <- checkbutton [ text "Press me", indicatorOn False
                        , indicatorColor "green", background "red"
                        ] w
     cb2 <- checkbutton [text "Wrong", width 8] w
     cset cb2 (command (pressed cb2))
     pack (cb1 ^-^ (l1 << cb2))

pressed :: Checkbutton -> GUI ()
pressed c =
  do b <- getValue c
     cset c (text (if b then "Right" else "Wrong"))
