
ex_message :: IO ()
ex_message = start $ 
  do w   <- window [title "What's the message?"]
     ms  <- binds
              [ message [ text msg, aspect (75*i), justify pos] w
              | pos  <- ["left","center", "right"], i <- [1..3] 
              ]
     pack (matrix 3 ms)
  where msg = "the message widget displays and formats a text"
