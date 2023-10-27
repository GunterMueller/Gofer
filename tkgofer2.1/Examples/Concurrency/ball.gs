
main :: IO ()
main = start $
  do w <- window [ title "Ball" ]
     c <- canvas [ width 300, height 200 ] w
     s <- button [ text "Start!", command (ball c) ] w
     q <- button [ text "Quit", command quit ] w
     pack (c ^^ (s << q))

ball :: Canvas -> GUI ()
ball can =
  do x <- random 260
     b <- coval (x+10,170) (x+30,190) [fillColor "red"] can
     animate (bouncing b)

animate :: [GUI ()] -> GUI ()
animate = fork . seqs . map (`seq` updateTask)

bouncing :: COval -> [GUI ()]
bouncing ball = [moveObject ball (0,y) | y <- alternate [1..16]]

alternate :: [Int] -> [Int] 
alternate [] = []
alternate xs = map ((-1)*) xs ++ xs ++ alternate (tail xs)

