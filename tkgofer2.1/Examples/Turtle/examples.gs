-- turtle examples ------------------------------------------------

line :: Int -> Picture
line n = copy n step

triangle :: Int -> Picture
triangle n = rep 3 (line n ++ [turn 120])

square :: Int -> Picture
square n = rep 4 (line n++[right])

dline :: Int -> Picture
dline n = rep (n/2) (step:blank)

circle :: Int -> Picture
circle n = scale 1 : concat [[turn n , step] | i <- [0,n..360]]

easy1 :: Picture
easy1 = [step, right, step, left, step]

easy2 :: Picture
easy2 = easy1 ++ easy1

easy3 :: Picture
easy3 = jump easy1 : easy1

fractal :: Picture -> Picture -> Int -> Picture
fractal p0 xs 0 = p0
fractal p0 xs n = concat [x:fractal p0 xs (n-1)| x <- xs] 

f1 = fractal [step] [id, right, left, left, right]
f2 = fractal [step] [step, right, left, left, right]
f3 = fractal [step] [id,turn 300,turn 120,turn 300]
f4 = fractal [step] [right, left , left, right, right]
f5 = fractal [id] [step,right, step, right]
f6 = fractal [id] [step, step, right]
f7 = fractal [step] [id,turn 5]
f8 = fractal [step, step] [step, right, left, left, right]
f9 = fractal (f3 2) [step, right, left, left, right]

peano :: Picture
peano = scale 10:rep 2 blank ++ peano' (10,0) 4
  where peano' (x,y) 0 = []
        peano' (x,y) n = 
          peano' (-y,-x) (n-1) ++ [move (y,x)] ++
          peano' (x,y) (n-1)   ++ [move (-x,-y)] ++
          peano' (x,y) (n-1)   ++ [move (-y,-x)] ++
          peano' (y,x) (n-1)   

snow :: Int -> Picture
snow = fractal [step] [id,turn 60,turn 240,turn 60]
 
mondriaan 0 = []
mondriaan n = step:line n ++ right : line n
                   ++ right : line n
                   ++ step: right : mondriaan (n-1)
 
spiral :: Int -> Picture
spiral m = scale 1: concat [ line (n/2) ++ [right] | n <- [m,m-1..1]]

