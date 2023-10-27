 

ex_label :: IO ()
ex_label = start $
  do w <- window [title "My Example"]
     l <- label [text "hello world", background "yellow", width 25] w
     pack l


hello = (start . openWindow [title "My Example"])
  (label [text "hello world", background "yellow", width 25])

