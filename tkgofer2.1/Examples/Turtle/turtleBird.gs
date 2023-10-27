-- ========================================================================= 
-- Turtle Graphics 
-- Ton Vullinghs, April 97
-- taken from Bird/Wadler, Introduction to Functional Programming
-- ========================================================================= 


-- to start, type: x `picturename' , e.g. x peano

-- display turtle ------------------------------------------

x :: Picture -> IO ()
x = display . draw


-- define turtle -------------------------------------------

type Turtle    = (Direction, Pen, Point)
type Direction = (Int, Int)               -- (angle,scale)
type Pen       = Bool                     -- pen down/up
type Point     = (Int, Int)               -- actual position
type Line      = ((Int,Int),(Int,Int))

type Command   = Turtle -> Turtle
type Picture   = [Command]

-- turtle commands -----------------------------------------

right, left, up, down :: Command
right = turn 90
left  = turn 270

up    (d, p, o)           = (d, False, o)
down  (d, p, o)           = (d, True, o)

scale :: Int -> Command
scale s ((i,j),p,o) = ((i,s),p,o)

turn :: Int -> Command 
turn d ((i,j),p,o) = (((i+d) `mod` 360,j), p, o) 

move :: (Int,Int) -> Command
move (x,y) ((a,s),b,(i,j)) = ((a,s),b, (i+x*s, j+y*s))

step :: Command
step  ((i, j), p, (x, y)) = 
  ((i, j), p, ( x + (mycos i * j), y + (mysin i * j)))
  where
     mysin     = trunc 2 . sin . rad
     mycos     = trunc 2 . cos . rad 
     rad x     = pi * (fromInteger x / 180.0) 
     trunc i r = truncate (fromInteger (10 ^ i) * r)

-- derived turtle commands ---------------------------------

skip :: Command
skip = id

thenT :: Command -> Command -> Command
f `thenT` g = g . f

back :: Command
back = right `thenT` right

jump :: [Command] -> Command
jump = foldl thenT skip            -- or foldr ??, important !!

-- picture commands ----------------------------------------

draw :: Picture -> Picture
draw p = [down] ++ p ++ [up]

blank :: Picture
blank = [up,step,down]

-- list processing functions --------------------------------

split     :: (a -> Bool) -> [a] -> [[a]]
split p s = 
  case dropWhile p s of
    [] -> []
    s' -> w : split p s''
          where (w,s'') = break p s'

rep :: Int -> [a] -> [a]
rep n = concat . copy n


-- converting pictures to lines ----------------------------

lineIt :: Picture -> [Line]
lineIt =  polyline . adjust . trail . turtle

turtle :: Picture -> [Turtle]
turtle = scanl (\x f -> f x) ((0,2),False,(0,0))

trail :: [Turtle] -> [[Point]]
trail = map (map thd3) . split (not . snd3)   

adjust :: [[Point]] -> [[(Int,Int)]]
adjust = map (map f) where f (x,y) = (x/10 + 10, y/10 + 10)

polyline :: [[(Int,Int)]] -> [Line]
polyline pss = concat [zip ps (tail ps) | ps <- pss]

-- setup graphic world -------------------------------------

graph :: [Line] -> IO ()  
graph cs = start $ do
  w <- window [ title "...Gofer-Turtle..."] 
  c <- canvas [ background "white"
              , width 300
              , height 300
              ] w
  pack c
  seqs [ cline x y [] c `seq` updateTask  | (x,y) <- cs ]
  tk_putTcl ["puts ready"]

display :: Picture -> IO ()
display = graph . lineIt


