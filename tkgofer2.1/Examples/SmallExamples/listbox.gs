
ex_listbox :: IO ()
ex_listbox = start $
  do w   <- window []
     l1  <- label [text "Strings"] w
     l2  <- label [text "Integers"] w
     lb1 <- listbox [initValue (part 3 ['A'..'Z'])] w
     lb2 <- listbox [initValue [1..8], multipleSelect True] w
     pack ((l1 ^-^ lb1) <|< (l2 ^-^ lb2))
  where part n = map (take n) . takeWhile (not . null) . iterate (drop n)
