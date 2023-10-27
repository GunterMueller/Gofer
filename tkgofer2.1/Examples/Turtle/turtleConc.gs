--------------------------------------------------------------
-- Simple Turtle Example
-- Koen Claessen, 1997
--------------------------------------------------------------

main = start $ doCommand exTree

-- examples

exTri :: Command
exTri = sub [ rr 100, rt 90, rr 160, tri 400 ]
 where
  tri :: Int -> Command
  tri n
    | n < 10    = fw n
    | otherwise = sub [ tri (n/3), lt 60
      		      , tri (n/3), rt 120
      		      , tri (n/3), lt 60
      		      , tri (n/3)
      		      ]

exTree :: Command
exTree = sub [ rr 180, tree 90 colors ]
 where
  tree :: Int -> [String] -> Command
  tree n (c:cs)
    | n < 5     = nop
    | otherwise = sub [ color c, fw n
      		      , lt x, tree'
      		      , rt (x+y)
      		      , tree'
      		      , lt y, rr n
      		      ]
   where
    tree' = tree ((n*3)/4) cs
    x     = 30
    y     = 40

exTree' :: Command
exTree' = sub [ rr 180, tree 90 colors ]
 where
  tree :: Int -> [String] -> Command
  tree n (c:cs)
    | n < 5     = nop
    | otherwise = sub [ color c, fw n
      		      , forked [sub [lt x, tree'], sub [rt y, tree']]
      		      ]
   where
    tree' = tree ((n*3)/4) cs
    x     = 30
    y     = 40

exStar :: Command
exStar = sub $ [ rr 180, rt 90, rr 40, lt 90 ]
            ++ [ sub [color c, fw 370, rt 157] | c <- colors ]

exArjan :: Command
exArjan = forked [ turn lt 180.0 7.0 colors
                 , turn rt 180.0 7.0 colors
                 , sub [ rt 180, turn lt 180.0 7.0 colors ]
                 , sub [ lt 180, turn rt 180.0 7.0 colors ]
                 ]
 where
  turn rot size step (c:cs)
    | size > step = sub [ color c
                        , rept 4 [ fw size, rot 90.0 ]
                        , fw step
                        , rot angle
                        , turn rot newsize step cs
                        ]
    | otherwise   = nop
    where
      newsize = sqrt (size^2 - 2.0 * size * step + 2.0 * step^2)
      angle   = (atan ( step / ( size-step ) ) * 180.0) / pi 

colors :: [String]
colors = cycle [ rgb r g b | (r,g,b) <- cs ]
 where
  cs = [ (255-x,x,0) | x <- ss ]
    ++ [ (0,255-x,x) | x <- ss ]
    ++ [ (x,0,255-x) | x <- ss ]
  ss = [0,20..255]

--------------------------------------------------------------
-- Implementation

-- Ints, Floats

class Number a where
  number :: a -> Float

instance Number Int where
  number = fromInteger
  
instance Number Float where
  number = id

-- commands

data Command
  = Rotate Float
  | Forward Float
  | Pen Bool
  | Color String
  | Commands [Command]
  | Forked Command Command

rt, lt, fw, bw, rr :: Number a => a -> Command
rt = Rotate . negate . number
lt = Rotate . number
fw = Forward . number
bw n = sub [rt 180, fw n, lt 180]
rr n = sub [pen False, bw n, pen True]

rept :: Int -> [Command] -> Command
rept n cs = Commands (concat (copy n cs))

sub :: [Command] -> Command
sub cs = Commands cs

pen :: Bool -> Command
pen b = Pen b

color :: String -> Command
color s = Color s

forked :: [Command] -> Command
forked cs = foldr1 Forked cs

nop :: Command
nop = Commands []

-- turtle

type Turtle = (Float, Float, Float, Bool, String)

startTurtle :: Turtle
startTurtle = (200.0,200.0,90.0,True, "black")

-- interpretation

doTurtle :: Canvas -> Turtle -> [Command] -> GUI ()
doTurtle can tur             []     = done
doTurtle can tur@(x,y,r,p,s) (c:cs) =
  case c of
    Rotate d     -> doTurtle can (x,y,r+d,p,s) cs
    Pen p'       -> doTurtle can (x,y,r,p',s) cs
    Color s'     -> doTurtle can (x,y,r,p,s') cs
    Commands cs' -> doTurtle can tur (cs' ++ cs)
    Forward n    -> forward n
    Forked c1 c2 -> forked c1 c2
 where
  forward n
    | not p     = doTurtle can (x',y',r,p,s) cs
    | otherwise = do cline (truncate x, truncate y)
                           (truncate x',truncate y') [penColor s] can
                     updateTask
                     doTurtle can (x',y',r,p,s) cs
    where
      x' = x + dir cos r n
      y' = y + dir (negate.sin) r n
      dir tri r n = n * tri ((r * pi) / 180.0)

  forked c1 c2 =
    do fork (doTurtle can tur [c2])
       doTurtle can tur [c1]

doCommand :: Command -> GUI ()
doCommand c =
  do win <- window [ title "Turtle" ]
     can <- canvas [ width 400, height 400 ] win
     but <- button [ text "Quit", command quit ] win
     pack ( can ^^ but )
     fork $ doTurtle can startTurtle [c]

--------------------------------------------------------------
-- the end.

