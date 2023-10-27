
ex_scrollbar :: IO ()
ex_scrollbar = start $
  do w <- window [title "select"]
     (e,f1) <- scrollEntry w
     (l,f2) <- scrollListbox w
     cset e (on return (readEntry l e))
     cset l (on (doubleClick 1) (writeEntry l e))
     focus e
     pack (f2 ^-^ f1)
  where
    scrollEntry w =
      do e <- entry [initValue ""] w
         s <- hscroll [] e
         result (e, e ^-^ s)
    scrollListbox w =
      do l <- listbox [] w
         s1 <- hscroll [] l
         s2 <- vscroll [] l
         result (l, (l ^-^ s1) <|< s2)
    readEntry l e = 
      do x <- getValue e
         putEnd l [x]
    writeEntry l e = 
      do [x] <- getSelection l
         [a] <- getFromTo l x x
         setValue e a
