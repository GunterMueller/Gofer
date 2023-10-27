-- nqueens simulation
-- start with `main'

-- setup the gui -----------------------------------------------

main :: IO ()
main = start $ do
  w  <- window [title "n-queens (n <= 8) "]
  ls <- binds [label [ background c1, foreground c2
                     , bitmap dummy, relief "raised"
                     ] w 
              | (c1,c2) <- colors
              ] 
  e  <- input [initValue 0, font "12x24"
              , foreground "red", background "yellow", width 8
              ] w
  cset e (on return (showQueens ls e))
  pack (matrix 8 ls ^*-^ e)


-- generate colors ---------------------------------------------

colors :: [(String, String)]
colors = zip (take 64 a) (take 64 (drop 8 a)) where
  a    = (concat . repeat) (take 8 (tail row) ++  take 8 row) 
  row  = (concat . repeat) ["blue","white"] 

-- and simulate... ---------------------------------------------

showQueens :: [Label] -> Input Int -> GUI ()
showQueens ls e = do 
  v <- getValue e
  cset e (text "0")
  let w = min v 8
  seqs [ act as | as <- queens w w ]
 where 
  act as = do
    board ls (zipWith (\a b -> a + b*8) as [0..7])
    i <- cget e text 
    cset e (text (show (numval i + 1)))
    updateTask
  board ls xs = seqs 
    [cset (ls !! i) (bitmap (if i `elem` xs then queen else dummy)) 
    | i <- [0..63]
    ]

dummy = "Bitmaps/dummy64.bm"
queen = "Bitmaps/q64s.bm"


-- finally, the nqueens algorithm (from standard gofer distribution) -

queens 0     c    = [[]]
queens (m+1) c    = [ p++[n] | p<-queens m c, n<-[0..c-1], safe p n ]

safe p n          = all not [ check (i,j) (m,n) | (i,j) <- zip [1..] p ]
                    where m = 1 + length p

check (i,j) (m,n) = j==n || (i+j==m+n) || (i-j==m-n)
