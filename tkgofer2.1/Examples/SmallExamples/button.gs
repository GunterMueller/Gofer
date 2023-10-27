-- application of buttons


ex_button :: IO ()
ex_button = start $
  do w <- window [title "My Example2"]
     l <- label  [text "hello world", background "yellow"] w
     b <- button [text "press me", command quit] w
     pack (l << b)
