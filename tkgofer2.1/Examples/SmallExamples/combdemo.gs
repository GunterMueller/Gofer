------------------------------------------------------------------------
-- Combinator Tutor
--
-- Thilo Schwinn, Ton Vullinghs
-- 11/30/95 Department 'Programmiermethodik' University of Ulm
--
-- start with `main'
--
------------------------------------------------------------------------

-- Packer type definitions --------------------------------------------

type TLayout = TTag Layout
data TTag a   = One a | Two a

type Size    = (Float,Float)
type Widget3 = RoseTree ([TLayout], Size, Size)

data Layout = Expnd | FX | FY | Hor | Ver | Grp

-- The Packer ---------------------------------------------------------

untag :: Eq a => [TTag a] -> [a]
untag = nub . map (\x  -> case x of (One l) -> l; (Two l) -> l)

instance Eq TLayout where
  One a  == One b  = a == b
  Two a  == Two b  = a == b
  _      == _      = False

instance Eq Layout where
  Expnd == Expnd = True
  FX  == FX  = True
  FY  == FY  = True
  Hor    == Hor    = True
  Ver    == Ver    = True
  Grp  == Grp  = True
  _      == _      = False

infixl 6 >>, >*>, >->, >|>, >+>, >*->, >*|>, >*+>
infixl 7 ##, #*#, #-#, #|#, #+#, #*-#, #*|#, #*+#

(>>)  = combine [Hor] 
(>->) = combine [Hor, FX] 
(>|>) = combine [Hor, FY]
(>+>) = combine [Hor, FX, FY]
(>*>) = combine [Hor, Expnd] 
(>*|>) = combine [Hor, Expnd, FY] 
(>*->) = combine [Hor, Expnd, FX] 
(>*+>) = combine [Hor, Expnd, FY, FX] 

(##)  = combine [Ver] 
(#-#) = combine [Ver, FX] 
(#|#) = combine [Ver, FY]
(#+#) = combine [Ver, FX, FY]
(#*#) = combine [Ver, Expnd] 
(#*|#) = combine [Ver, Expnd, FY] 
(#*-#) = combine [Ver, Expnd, FX] 
(#*+#) = combine [Ver, Expnd, FY, FX] 

expnd :: Widget3 -> Widget3
expnd = add [Expnd] 
fX  = add [FX] 
fY  = add [FY] 
fXY  = add [FX, FY] 
grp  = add [Grp]

combine :: [Layout] -> Widget3 -> Widget3 -> Widget3
combine s a b = Rose ([],occ, occ) (map (rewsize occ) sons)
  where 
    sons = (lift s a ++ lift s b) 
    occ  = wsize sons 

lift :: [Layout] -> Widget3 -> [Widget3]
lift s (Rose ([],_,_) (w:ws))  
  | all (\(Rose (ls,_,_) ws) -> [a | Two a <- ls] == s) (w:ws) = (w:ws)
lift s (Rose (ls, o, i) ws)  
  | otherwise = [Rose (nub (ls ++ map Two s), o, i) ws]

wsize = foldr (\(Rose (l, (x,y),_) _) (x',y') ->
  if (Two Hor) `elem` l then (x'+x, max y' y) 
                        else (max x' x, y'+y)) (0.0,0.0) 

rewsize (x,y) (Rose (l, o, (xi,yi)) ws) = 
  Rose (l,o, if (Two Hor) `elem` l then (xi,y) else (x,yi)) ws
 

add :: [Layout] -> Widget3 -> Widget3
add s (Rose (ls,o,i) ws) = Rose (nub (ls ++ map One s), o,i) ws

-- --------------------------------------------------------------

widget :: Size -> Widget3
widget x = Rose ([],x ,x) []


fiexp :: Widget3 -> Widget3
fiexp w = 
   let (Rose (l',o',i') ws') = layout w
   in (Rose (l',o',i') (map fiexp ws'))

doF :: Widget3 -> Widget3
doF (Rose (l, occ, (xi,yi)) w) =
   Rose (l, (foldr (.) id (map f (untag l))) occ, (xi,yi)) w
   where f FX = \(a,b) -> (xi,b)
         f FY = \(a,b) -> (a,yi)
         f _     = id

doExpnd :: Widget3 -> Widget3
doExpnd (Rose (l,(xo,yo),(xi,yi)) w) =
  Rose (l, (xo, yo), (xi, yi)) (g ((xo-xc) / n ,  (yo - yc) / n) w)
  where n       = sum [1.0 | Rose (l,o,i) ws <- w, Expnd `elem` untag l]
        g (x,y) = map (\(Rose (l,o,(x1,y1)) ws) ->
                      if Expnd `elem` untag l then Rose (l,o,(x1+x,y1+y)) ws
                                              else Rose (l,o,(x1,y1)    ) ws)
        (xc,yc) = wsize w


layout = doExpnd . doF

type Point     = (Float, Float)
type Rec       = (Point, Point)
type Rect = RoseTree (Rec, Rec) 

------------------------------------------------------------------

rect :: Widget3 -> Rect
rect w = head (mkrec (0.0,0.0) [w])

mkrec :: Point -> [Widget3] -> [Rect]
mkrec p0 [] = []
mkrec (x,y) ((Rose (ps, (o1,o2), (i1,i2)) ws):bs) =
  (Rose ((p0,p1),(p2,p3)) (mkrec p2 ws)) : (mkrec p4 bs)
  where c1      = (i1 - o1) / 2.0
        c2      = (i2 - o2) / 2.0
        p0      = (x, y)
        p1      = (x + i1, y + i2)
        p2      = (x + c1, y + c2)
        p3      = (x + c1 + o1, y + c2 + o2)
        p4      = if ((Two Hor) `elem` ps) then (x + i1, y) else (x, y + i2)
 
 
locate :: (Rect -> [Rec]) -> Widget3 -> [Rec]
locate f w = (f . rect . fiexp) w

atomrec :: Rect -> [Rec]
atomrec (Rose (a,b) []) = [b]
atomrec (Rose (a,b) rs) = concat (map atomrec rs)

inhrec  :: Rect -> [Rec]
inhrec (Rose (a, b) rs) = a : concat (map inhrec rs)

occrec  :: Rect -> [Rec]
occrec (Rose (a, b) rs) = b : concat (map occrec rs)

drawrec :: (Rect -> [Rec]) -> Widget3 -> [((Int,Int),(Int,Int))]
drawrec f w = map adj (locate f w) 
  where adj ((a,b),(c,d)) = let (x1,y1) = (r2i a, r2i b)
                                (x2,y2) = (r2i c, r2i d)
                            in ((x1+1,y1+1), (x2-1,y2-1))
        r2i r = truncate (8.0*r+50.0)




-- GUI type definitions ----------------------------------------------

type CState = Clipboard (Canvas, (Listbox [String]), [(Widget3,String)])

-- main program & GUI -------------------------------------------------

main :: IO ()
main = start tutor

tutor :: GUI ()
tutor = 
  do win    <- create_window 
     (f1,b) <- create_board win
     (f2,l) <- create_widlist win
     s      <- create_state b l
     create_binding s l
     f3     <- create_control win l s
     (pack . flexible) (flexible (flexible f1 <|< f2) ^-^ f3) 

 where

    create_window =
      windowDefault [title "Widget Combinators"] 
                    [large, background "lightblue"]

    create_board w = do
      board <- canvas [ scrollRegion (1000,1000)
                      , background "lavender"
                      ] w
      s1 <- hscroll [] board
      s2 <- vscroll [] board
      f  <- frame [] (flexible (flexible (flexible board ^-^ s1) <|< s2))
      result(f,board)

    create_widlist w = do
      l  <- label [text "Widgets", large] w
      ls <- listbox [ multipleSelect True
                    , initValue ["A","B","C"]
                    , width 10
                    , background "honeydew"
                    ] w
      h  <- hscroll [] ls
      v  <- vscroll [] ls
      f  <- frame [] (flexible (flexible (flexible ls ^-^ h) <|< v))
      result(l ^-^ f, ls)
  
    create_binding s l =
      cset l (on (doubleClick 1) $
              do (x:xs)   <- getSelection l
                 (c,w,ll) <- getValue s
                 showWidget c (fst (ll !! x))
             )

    create_state b l = 
      clipboard [ initValue (b, l, initwids) ]

    create_control w l s = do
      l    <- label [text "Control"] w
      b    <- button [text "New Widget", command (newwidget s)] w
      bs   <- buttons s w 
      result (l ^-^ (bs << b))


initwids :: [(Widget3,String)]
initwids = zip [a,b,c] ["A","B","C"]
  where a = widget (4.0,10.0)
        b = widget (6.0,6.0)
        c = widget (12.0,2.0)


buttons :: CState -> Window -> GUI Frame
buttons st w = 
  do combs <- binds [mkbut s (foldr1 c) (foldr1 (\a b -> a ++ s ++ b))  
                    | (s,c) <- (vbut++hbut)]
     cmds  <- binds [mkbut s (c . head) ((++) s . head) 
                    | (s,c) <- obut]
     result (matrix 4 combs <*|< matrix 2 (map flexible cmds))

  where 

   mkbut s f g = button [ width 5
                        , text s 
                        , command (appl st f g)
                        , background "moccasin"
                        ] w
   opts1 = [("",[]), ("-",[FX]), ("|",[FY]), ("+", [FX,FY])]
   opts2 = [('*':s, Expnd:os) | (s,os) <- opts1]
   hbut  = [(("<" ++ s ++ "<"), combine (Hor:os)) | (s,os) <- (opts1 ++ opts2)]
   vbut  = [(("^" ++ s ++ "^"), combine (Ver:os)) | (s,os) <- (opts1 ++ opts2)]
   obut  = [(s, add os) | (s,os) <- tail (opts1++opts2)]

       
appl st f g = 
  do (c,wids, ws) <- getValue st
     ns <- getSelection wids
     if ns == [] then done else 
          let (wtrm,wstr) = unzip [ws !! i | i <- ns]
              ntrm        = f wtrm
              nstr        = g wstr
          in do putEnd wids [nstr]
                setValue st (c,wids, ws ++ [(ntrm,nstr)])
                showWidget c ntrm

showWidget :: Canvas -> Widget3 -> GUI ()
showWidget d w = 
   do clearCanvas d
      z <- binds [rectangle' p1 p2 [fillColor "white"] d
                 | (p1,p2) <- drawrec occrec w
                 ]
      x <- binds [rectangle' p1 p2 [fillColor "seashell"] d
                 | (p1,p2) <- drawrec atomrec w
                 ]
      y <- binds [rectangle' p1 p2 [penColor "blue"] d
                 | (p1,p2) <- drawrec inhrec w
                 ]
      done

rectangle' d p1 p2 cs =
  do x <- crect d p1 p2 cs
     updateTask
     result x
     

newwidget :: CState -> GUI ()
newwidget st =
  do w <- windowDefault [title "Create"] [large]
     (c,l,ws) <- getValue st
     s1 <- hscale [scaleRange (1,30), text "X", relief "raised"] w
     s2 <- hscale [scaleRange (1,30), text "Y", relief "raised"] w
     cset s1 (command (showNew s1 s2 c))
     cset s2 (command (showNew s1 s2 c))
     l1 <- label [text "Name:"] w
     e  <- entry [initValue ("w"++show (length ws)), width 5] w
     b1 <- button [text "Cancel", command (closeWindow w)] w
     b2 <- button [text "Ok", command $ 
                     do x <- getValue s1
                        y <- getValue s2
                        n <- getValue e
                        setValue st (c,l, 
                          ws ++ [(widget ( primIntToFloat x
                                         , primIntToFloat y
                                         ), n)])
                        putEnd l [n]
                        closeWindow w
                  ] w
     pack ((s1 <|< s2) ^-^ (l1 <-< e) ^-^ (b1 <-< b2)) 
                 
   
showNew s t c = 
  do clearCanvas c
     x <- getValue s
     y <- getValue t
     showWidget c (widget (primIntToFloat x, primIntToFloat y))
    

large :: HasForeground c => Conf c
large = font "LucidaSans18"



