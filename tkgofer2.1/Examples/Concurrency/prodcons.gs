
producer :: Channel String -> FilePath -> GUI ()
producer chan file =
  do xs <- readFileLines file
     seqs [ chan =: x | x <- xs ]

consumer :: Channel String -> (String -> GUI ()) -> GUI ()
consumer chan f =
  do x <- readV chan
     f x
     updateTask
     consumer chan f

main :: IO ()
main = start $
  do w <- window [ title "Producer - Consumer" ]
     e <- edit [ height 30, width 80 ] w
     b <- button [ text "Quit", command quit ] w
     pack (e ^^ b)
     
     chan <- newChan'
     fork (producer chan "prodcons.gs")
     fork (consumer chan (putEnd e . (++"\n")))
