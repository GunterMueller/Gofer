-- turtle.gs
-- simple peano curve
-- start with `main'


           
-- ---Turtle ---------------------------------------------


data Direction = No | So | We | Ea

type Turtle = Clipboard (Int,Int,Direction, Canvas)

------------------------- main program ------------------------------------

main :: IO ()
main = start $ 
  do {w <- windowDefault [title "Follow me, if you can"] 
                         [font "12x24", background "lightblue"]
     ; c   <- canvas [background "yellow",scrollRegion (500,500)] w
     ; sc1 <- vscroll [] c
     ; sc2 <- hscroll [] c
     ; st <- clipboard [ initValue (20, 20, Ea, c) ]
     ; bs <- let but i = button [ text ("n = " ++ (show i))
                                , command (do {reset st; peano st i Ea})
                                ] w
             in binds [but i | i <- [1..5]]
     ; pack (flexible (matrix 5 bs ^+^ 
             expand ( expand (expand c ^+^ sc2) <+< sc1)))
     }

reset st =
  do { (_,_,_,c) <- getValue st
     ; setValue st (20,20,Ea,c)
     ; clearCanvas c
     }

peano :: Turtle -> Int -> Direction -> GUI ()
peano s 0 d = done
peano s n d =
  case d of 
    No -> draw s n Ea No We
    So -> draw s n We So Ea
    We -> draw s n So We No
    Ea -> draw s n No Ea So

draw :: Turtle -> Int -> Direction -> Direction -> Direction -> GUI ()
draw s n d1 d2 d3 =
  do { peano s (n-1) d1 
     ; drawline s d1 
     ; peano s (n-1) d2 
     ; drawline s d2 
     ; peano s (n-1) d2 
     ; drawline s d3  
     ; peano s (n-1) d3 
     }

drawline :: Turtle -> Direction -> GUI ()
drawline st dir =
  do { (x,y,d,c) <- getValue st
     ; let (x2,y2) = (case dir of
                           So  -> (x,y-10)
                           No  -> (x,y+10)
                           We   -> (x-10,y)
                           Ea   -> (x+10,y))
       in do { l <- cline (x,y) (x2,y2) [penWidth 3] c
             ; setValue st (x2,y2,dir,c)
             ; updateTask
             }
     }

 
-------------------------------------- end -------------------------------------

