-- Indicator Bar ---------------------------------------
-- combination on a Canvas
        
type IndState = GVar (Int,Int,Int)               -- value, width, height
type IndGUI   = (Canvas, Label, CRect, CRect)   -- canvas, border, indicator

data Indicator0 a = Indicator IndGUI IndState a
type Indicator    = WItem (Indicator0 Int)

canI :: Indicator -> Canvas
canI i = let (Indicator (c,_,_,_) _ _) = getWidget i in c

labI :: Indicator -> Label
labI i = let (Indicator (_,l,_,_) _ _) = getWidget i in l

borI :: Indicator -> CRect
borI i = let (Indicator (_,_,b,_) _ _) = getWidget i in b

recI :: Indicator -> CRect
recI i = let (Indicator (_,_,_,r) _ _) = getWidget i in r

stateI :: Indicator -> IndState
stateI i = let (Indicator _ st _) = getWidget i in st

indicator :: [Conf Indicator] -> Window -> GUI (Indicator)
indicator cs w =
  let defaults  = [height 20, width 100, foreground "red"]
  in
  do c <- canvas [] w
     l <- label [width 4, text "0%"] w
     i <- crect (0,0) (0,0) [] c
     j <- crect (0,0) (0,0) [] c
     st <- newGVar (0,20,100)
     composeWidget (Indicator (c,l,i,j) st 0) (c<|<l) (defaults ++ cs)
 
instance Widget Indicator where
  cset w c =
    case (c w) of
        Tk_Height h     -> newheight w h
        Tk_Width h      -> newwidth w h
        Tk_Foreground r -> cset (recI w) (fillColor r)
        Tk_Background r -> do cset (labI w) (background r)
                              cset (canI w) (background r)
                              cset (canI w) (highlightBackground r)
        Tk_Font f       -> cset (labI w) (font f)
        otherwise       -> cset (canI w) (const (c w))
    where
      newheight ind v =
        do (i,x,y) <- readGVar (stateI ind)
           writeGVar (stateI ind) (i,x,v)
           let ratio = (fromInteger x / 100.0) * fromInteger i
           setCoords (borI ind) [(3,3),(x+7,v+3)]
           setCoords (recI ind) [(5,5),(5+truncate ratio,5+(v-4))]
           cset (canI ind) (height (v+4))
       
      newwidth ind v =
        do (i,x,y) <- readGVar  (stateI ind)
           writeGVar (stateI ind) (i,v,y)
           let ratio = (fromInteger v / 100.0) * fromInteger i
           setCoords (borI ind) [(3,3), (v+7,y+3)]
           setCoords (recI ind) [(5,5), (5+truncate ratio,5+(y-4))]
           cset (canI ind) (width (v+8))


instance HasBackground Indicator
instance HasForeground Indicator 
instance HasBorder     Indicator
instance HasWidth      Indicator
instance HasHeight     Indicator


instance HasInput WItem Indicator0 Int where
  getValue w = do (i,_,_) <- readGVar (stateI w)
                  result i
 
  setValue w i =
    do (v,x,y) <- readGVar (stateI w)
       writeGVar (stateI w) (i,x,y)
       let newx = truncate ((fromInteger x / 100.0) * fromInteger i)
       setCoords (recI w) [(5,5), (5+newx,5+(y-4))]
       cset (labI w) (text (show i ++ "%"))

