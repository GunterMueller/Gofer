
---------------------------------------------------------------
-- lift.gs : 1-lift simulator
--
-- start with `main'
---------------------------------------------------------------

type Floor   = Int
type Reqs    = [(Floor,Dir)]
data Door    = Open | Closed
data Dir     = Up | Down
type LState  = Clipboard (Motor, Control, Timer)
type Control = (Floor,Dir,Door,Reqs)
type Motor   = ([Button], [Button], [Button])
type DoorW   = (CRect, CRect, Button)


-- start main loop --------------------------------------------

main :: IO ()
main = start $ 
  do clk        <- timer [ initValue 100, active False ]
     lift       <- clipboard [ initValue (([],[],[]), (0,Up,Open,[]),clk)]
     d          <- door              
     (r1,r2,c)  <- building lift
     p          <- panel lift c d
     (_,s,c)    <- getValue lift
     setValue lift ((p, r1, r2),s,c)

floors = reverse [(maxf Down)..(maxf Up)]

pans w st = 
  [ button [width 5, text (show f), command (dogoto st f)] w
  | f <- floors
  ]

reqs d w st c = 
  [ if (f == maxf d)
    then button [width 5, relief "flat"] w
    else button [width 5, text (show d), command (dorequest st d f)] w
  | f <- floors
  ] 

instance Eq Dir where
  Up   == Up   = True
  Down == Down = True
  _    == _    = False

instance Text Dir where
  showsPrec d Up   = showString "^"
  showsPrec d Down = showString "v"

door =
  do w3 <- window [title "Door"]
     dd <- canvas [width 100, height 100, background "honeydew"] w3
     crect (25,15) (75,85) [fillColor "lightyellow"] dd
     cline (0,0) (25,15) [] dd
     cline (0,100) (25,85) [] dd
     cline (100,0) (75,15) [] dd
     cline (100,100) (75,85) [] dd
     left  <- crect (-50,0) (5,100) [fillColor "lightblue"] dd
     right <- crect (95,0) (150,100) [fillColor "lightblue"] dd
     pack dd
     result (left,right)
  
panel st c (l,r) =
  do w2 <- windowDefault [title "Panel"] 
                         [font "helvetica24", background "SpringGreen2"]
     p  <- binds (pans w2 st)
     d <- button [text "><"] w2
     cset d (command (doclose st (l,r,d) c))
     pack (matrix 4 p ^-^ d)
     result (reverse p)

building st =
  do w <- windowDefault [title "The Lift"]
                        [font "helvetica24", background "SpringGreen2"]
     c  <- vscale [ scaleRange ((maxf Up),(maxf Down))
                  , initValue (maxf Down)
                  , background "honeydew"
                  , width 100
                  ] w
     r1 <- binds (reqs Up w st c)
     r2 <- binds (reqs Down w st c)
     pack ((c <|< horizontal (map (vertical) [r1,r2])))
     result (reverse r1, reverse r2,c)


-- Simulation ---------------------------------------------

dogoto :: LState -> Int -> GUI () 
dogoto st n = 
  do ((p,r1,r2),s,c) <- getValue st
     inactivate (p!!n) 
     setValue st ((p,r1,r2), goto n s,c)
    

goto r (f,dir,door,reqs) =
   if r > f then (f,dir,door,(r,Up):reqs) else
   if r < f then (f,dir,door,(r,Down):reqs) else
                 (f,dir,door,(r,dir):reqs)

doclose :: LState -> DoorW -> Scale -> GUI ()
doclose v (l,r,b) c = 
  do (g,(f,dir,door,reqs),clk) <- getValue  v
     cset clk (active False) 
     inactivate b 
     closeDoor l r
     csets clk [ command (donext v (l,r,b) c)
               , active True
               ]
     setValue v (g,(f,dir,Closed,reqs),clk)

dorequest :: LState -> Dir -> Floor -> GUI () 
dorequest v d n = 
  do (g,(f,dir,door,reqs),clk) <- getValue v
     inactivate ((sel d g) !! n) 
     setValue v (g,(f,dir,door,(n,d):reqs),clk)

donext :: LState -> DoorW -> Scale -> GUI ()
donext v b s =
  do (m,c,clk)  <- getValue v
     let (a,c') = donext' m c b s clk
     setValue v (m,c',clk)
     a

donext' :: Motor -> Control -> DoorW -> Scale -> Timer -> (GUI (), Control)
donext' _ (f,dir,door,[])   b s c = (done, (f,dir,door,[]))
donext' _ (f,dir,Open,reqs) b s c = (done, (f,dir,Open,reqs))
donext' (p,r1,r2) (f,dir,Closed,reqs) (l,r,b) s c
 = (open, (f,dir,Open,new_reqs))                        , (f,dir) `elem` reqs
 = (moveL, (f+one dir,dir,Closed,reqs))                 , future_reqs /= []
 = donext' (p,r1,r2) (f,swap dir,Closed,reqs) (l,r,b) s c , otherwise 
  where new_reqs    = filter (/=(f,dir)) reqs 
        future_reqs = filter (\x -> (next dir) (fst x) f) reqs  
        open = do if f == (maxf dir) then done 
                                     else activate ((sel dir (p,r1,r2)) !! f)
                  activate (p !! f)
                  openDoor l r
	          activate b        
		  cset c (active False)
        moveL = setValue s (f+one dir)

----------------------------------------------------------------

openDoor l r = closeDoor r l

closeDoor l r =
  seqs [do moveObject l (5,0)
           moveObject r (-5,0)
           updateTask
       | x <- [1..9]
       ]

sel Up   (p,r1,r2) = r1
sel Down (p,r1,r2) = r2

swap Down = Up
swap Up = Down

next Down = (<)
next Up = (>)

one Down = -1
one Up = 1

maxf Down = 0
maxf Up = 15

----------------------------------------------------------------

activate b =
 cset b (active True) `seq` 
 cset b (background "SpringGreen2")

inactivate b =
 cset b (active False) `seq`
 cset b (background "Red")

----------------------------------------------------------------

