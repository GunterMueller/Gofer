-- sort.gs
-- a demonstration of bubble sort and selection sort
--
-- start with `main'
--



------------------------------------------------------------   
-- define new graphical objects to represent integers

data OvalInt0 = OvalInt (COval, CText)
type OvalInt  = CItem OvalInt0

instance Widget OvalInt

instance HasCoords OvalInt where
  moveObject (CItem (OvalInt (o,t)) _ _) s 
    = do {moveObject o s; moveObject t s}

ovalInt :: Canvas -> (Int,Int) -> [Conf COval] -> Int -> GUI OvalInt
ovalInt c (x,y) cs i = 
  do o <- coval (x,y) (x+20,y+20) cs c
     t <- ctext (x+3,y+10) [text (show i)] c
     result (CItem (OvalInt (o,t)) "" 0)

------------------------------------------------------------   

data SortState = Clipboard 
               ( [Int]            -- the input sequence
               , [OvalInt]        -- its graphical representation
               , Int              -- sorted / unsorted index
               )

-- user interface defintion --------------------------------   

main :: IO ()
main = start $  
  do win        <- window [title "Sort-Demo"]
     (f1,c1,s1) <- make_world win "bubble sort" 
     (f2,c2,s2) <- make_world win "selection sort" 
     men        <- menu_bar win (c1,s1) (c2,s2)
     bar        <- button_bar win (c1,s1) (c2,s2) 
     inp        <- input [text "enter value:"] win
     cset inp (on return $ do val <- getValue inp
                              setValue inp 0
                              ins_elem c1 s1 val
                              ins_elem c2 s2 val
              )
     pack (men ^-^ (f1 <|< f2) ^-^ (inp <*-< bar))

make_world w s =
  do l <- label [text s] w
     c <- canvas [background "white", width 310, height 310] w
     f <- frame [borderWidth 4, relief "raised"] (l ^-^ c)
     s <- clipboard [ initValue ([],[],0) ]
     result (f,c,s)

button_bar w (c1,s1) (c2,s2) =
  do b1 <- button [ text "Step"
                  , command (doStep s1 s2)
                  ] w
     b2 <- button [ text "Run"
                  , command (doRun s1 s2)
                  ] w
     b3 <- button [ text "Reset"
                  , command (doReset c1 c2 s1 s2)
                  ] w
     frame [borderWidth 4, relief "flat"] (b1 <*-< b2 <*-< b3) 
    

menu_bar w b s =
  do mb  <- menubutton [text "Options"] w
     me  <- menu [] mb
     op1 <- mbutton [ text "reverse"
                    , command (doInp (reverse [0..14]) b s)
                    ] me
     op2 <- mbutton [ text "updown"
                    , command (doInp (reverse [8..14] ++ [0..7]) b s)
                    ] me
     sp  <- label [] w
     result (mb << flexible sp)
  where
    doInp xs (w1,s1) (w2,s2) =
       do seqs [ins_elem w1 s1 v | v <- xs]
          seqs [ins_elem w2 s2 v | v <- xs]

ins_elem w s v =
  do (vs,es,i) <- getValue s
     if (v >= 0) && (v < 15) && (length vs < 15) then
          do { e <- ovalInt w (20 * length vs +5, v * 20+5) 
                     [fillColor "yellow"] v
             ; setValue s (vs ++ [v], es ++ [e],i)
             }
       else done

-- Simulation ----------------------------------------------   

doRun st1 st2 =
 do (val1,elems,i) <- getValue st1
    (val2,elems,i) <- getValue st2
    if (is_sorted val1) && (is_sorted val2) then done
       else do doStep st1 st2 
               doRun st1 st2

doStep st1 st2 =
  do (a,b,i) <- getValue st1
     h2 <- getValue st2
     (a',b') <- bubbleSort (a,b)
     w2 <- selectionSort h2
     setValue st1 (a',b',i)
     setValue st2 w2

doReset w1 w2 st1 st2 =
  do clearCanvas w1; clearCanvas w2
     setValue st1 ([],[],0)
     setValue st2 ([],[],0)

-- display swap operation ----------------------------------

exchange i j es = 
  let e1 = es !! i
      e2 = es !! j
  in seqs [do moveObject e1 (4,0)
              moveObject e2 (-4,0)
              updateTask
          | tick <- [1..5*(j-i)]
          ]

-- bubble sort ---------------------------------------------   
 
bubbleSort :: ([Int],[OvalInt]) -> GUI ([Int], [OvalInt])
bubbleSort ([],[])   = result ([],[])
bubbleSort ([x],[e]) = result ([x],[e])
bubbleSort (x:y:xs, e1:e2:es) 
  = if x > y 
    then do {exchange 0 1 [e1,e2]
            ;result (y:x:xs,e2:e1:es)
            }
    else do {(xs',es') <- bubbleSort (y:xs, e2:es)
            ;result (x:xs',e1:es')
            }

-- selection sort ------------------------------------------   

selectionSort :: ([Int], [OvalInt], Int) -> GUI ([Int], [OvalInt], Int)
selectionSort ([],[],i)   = result ([],[],i)
selectionSort ([x],[e],i) = result ([],[],i)
selectionSort (xs,es,i) =
   let (sorted,  sorted_e)    = (take i xs, take i es)
       (unsorted, unsorted_e) = (drop i xs, drop i es)
       j = smallest_pos unsorted 
   in do {exchange 0 j unsorted_e
         ;let xs' = swapFirst j unsorted
              es' = swapFirst j unsorted_e
          in result (sorted ++ xs',sorted_e ++ es',i+1)
         }


swapFirst j []     = []        
swapFirst j (x:xs) = 
 let (ls,r:rs) = splitAt j (x:xs)
 in case ls of []     -> r:rs
               (y:ys) -> (r:ys) ++ (y:rs)

smallest_pos xs =
  let mi = foldl1 min xs
  in length (takeWhile (/=mi) xs) 

is_sorted []  = True
is_sorted [x] = True
is_sorted (x:y:xs) = x <= y && is_sorted (y:xs)
