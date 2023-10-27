
hello :: IO ()
hello = start $
  do w <- window []
     csets w [title "My Example", winSize (200,200), winPosition (10,10)]
     l <- label [text "hello world", background "yellow"] w
     pack l

