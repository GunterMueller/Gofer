-- The 15 Puzzle in 15 lines

main :: IO ()
main = start $ openDefault [title "puzzle" ] [background "yellow"] $ \w -> do
  bs <- binds [button [relief "ridge", width 3] w | i <- [0..15]]
  let shift a = do ts <- binds [cget w text | w <- bs]
                   let [b] = [i | (i,"") <- zip [0..] ts]
                       as | a `div` 4 == b `div` 4 = [f 1,f 2..a]
                          | a `mod` 4 == b `mod` 4 = [f 4,f 8..a]
                          | otherwise              = []
                       f i = if a > b then b+i else b-i
                   seqs [do csets (bs !! u) [text (ts !! v), background "gold"]
                            csets (bs !! v) [text "", background "yellow"] 
                        |(u,v) <- zip (b:as) as ]
  seqs [cset  (bs !! i) (command (shift i)) | i <- [0..15]]
  seqs [csets (bs !! i) [text (show i), background "gold"] | i <- [1..15]]
  result (matrix 4 bs) 
