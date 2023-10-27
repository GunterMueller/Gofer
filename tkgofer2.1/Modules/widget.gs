----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- widget.gs

-- need prelude.gs
-- need guiMonad.gs

--------------------------------------------------------------------------
-- The Widgets

-- Class Widget ------------------------------------

class TkWidget w => Widget w where
  cset   :: w -> Conf w -> GUI ()
  cget   :: GUIValue v => w -> (v -> Conf w) -> GUI v
 
  csets ::  w -> [Conf w] -> GUI ()
  csets w = seqs . map (cset w)
 
  onArgs :: TkEvent -> String -> ([String] -> GUI ()) -> Conf w

  on :: TkEvent -> GUI () -> Conf w
  on e a = onArgs e "" (\_ -> a)

  onxy :: TkEvent -> ((Int,Int) -> GUI ()) -> Conf w   -- relative to widget
  onxy c e = onArgs c "xy" (\[x,y] -> let xx = numval x 
                                          yy = numval y in e (xx,yy))
 
  onXY :: TkEvent -> ((Int,Int) -> GUI ()) -> Conf w   -- relative to screen
  onXY c e = onArgs c "XY" (\[x,y] -> let xx = numval x 
                                          yy = numval y in e (xx,yy))

  destroyEvent  :: w -> TkEvent -> GUI ()    -- destroy 1 event
  destroyEvents :: w -> GUI ()               -- destroy all events

-- Class HasBackground ---------------------------------
 
class Widget w => HasBackground w where
  background :: String -> Conf w
  background s _ = Tk_Background s

-- Class HasForeground ---------------------------------
 
class Widget w => HasForeground w where
  foreground :: String -> Conf w
  foreground s _ = Tk_Foreground s
 
  font :: String -> Conf w
  font s _ = Tk_Font s

-- Class HasBorder -------------------------------------
 
class Widget w => HasBorder w where
  borderWidth :: Int -> Conf w
  borderWidth i _ = Tk_BorderWidth i

  cursor :: String -> Conf w
  cursor s _ = Tk_Cursor s

  relief :: String -> Conf w  
  relief r _ = Tk_Relief r

-- Class HasWidth --------------------------------------
 
class Widget w => HasWidth w where
  width :: Int -> Conf w
  width s _ = Tk_Width s
 
  highlightBackground :: String -> Conf w
  highlightBackground s _ = Tk_HighlightBackground s
 
  highlightColor :: String -> Conf w
  highlightColor s _ = Tk_HighlightColor s
 
  highlightThickness :: Int -> Conf w
  highlightThickness i _ = Tk_HighlightThickness i
 
-- Class HasFocus --------------------------------------
 
class Widget w => HasFocus w where
  focus :: w -> GUI ()
  focus w = tk_putTcl ["focus", tk_getPathName w]
 
  takeFocus :: Bool -> Conf w
  takeFocus b _ = Tk_TakeFocus b

-- Class HasHeight -------------------------------------
 
class Widget w => HasHeight w where
  height :: Int -> Conf w
  height i _ = Tk_Height i

-- Class HasPad ----------------------------------------
 
class Widget w => HasPad w where
  padx :: Int -> Conf w
  padx i _ = Tk_PadX i
 
  pady :: Int -> Conf w
  pady i _ = Tk_PadY i

-- Class HasAnchor -------------------------------------
 
class Widget w => HasAnchor w where
  anchor :: String -> Conf w
  anchor s _ = Tk_Anchor s
 
  justify :: String -> Conf w
  justify s _ = Tk_Justify s

-- Class HasIndicator ----------------------------------
 
class Widget w => HasIndicator w where
  indicatorColor :: String -> Conf w
  indicatorColor s _ = Tk_IndicatorColor s
 
  indicatorOn :: Bool -> Conf w
  indicatorOn s _ = Tk_IndicatorOn s

-- Class HasCoords -------------------------------------
 
class Widget a => HasCoords a where
  moveObject   :: a -> (Int, Int) -> GUI ()
  removeObject :: a -> GUI ()
  lowerObject  :: a -> GUI ()
  raiseObject  :: a -> GUI ()
  getCoords    :: a -> GUI [(Int,Int)]
  setCoords    :: a -> [(Int,Int)] -> GUI ()

-- Class HasFillColor ----------------------------------
 
class HasCoords a => HasFillColor a where
  penWidth :: Int -> Conf a
  penWidth i _ = Tk_Width i
 
  penColor :: String -> Conf a
  penColor c _ = Tk_OutlineColor c
 
  fillColor ::  String -> Conf a
  fillColor c _ = Tk_FillColor c



-- Class HasScroll -------------------------------------
 
class Widget w => HasScroll w 

-- Class HasInput --------------------------------------
 
class Widget (c (w v)) => HasInput c w v where
  getValue :: c (w v) -> GUI v
  setValue :: c (w v) -> v -> GUI ()
 
  updValue :: (v -> v) -> c (w v) -> GUI ()
  updValue f x = do {i <- getValue x; setValue x (f i)}

  withValue ::  (v -> GUI b) -> c (w v) -> GUI b
  withValue f w = getValue w `bind` f 
 
  initValue :: v -> Conf (c (w v))
  initValue v a = Tk_InitValue (setValue a v)
 
  readOnly :: Bool -> Conf (c (w v))
  readOnly b a = Tk_Active (not b)
 
-- Class HasPosition -----------------------------------
 
class HasInput WItem (w p) v => HasPosition w p v where
 
  putBegin     :: WItem (w p v) -> v -> GUI ()
  putEnd       :: WItem (w p v) -> v -> GUI ()
  putPos       :: WItem (w p v) -> p -> v -> GUI ()
  getFromTo    :: WItem (w p v) -> p -> p -> GUI v
  getSize      :: WItem (w p v) -> GUI p
 
  delFromTo :: GUIValue p => (WItem (w p v)) -> p -> p -> GUI ()
  delFromTo w p1 p2 = tk_toTcl w ["delete", tk_toGUI p1, tk_toGUI p2]
 
  setYView :: WItem (w p v) -> Int -> GUI ()
  setYView w i = tk_toTcl w ["yview", show i]

  getSelection :: (WItem (w p v)) -> GUI [p]
  setSelection :: (WItem (w p v)) -> [p] -> GUI ()
 
  selectBackground  :: String -> Conf (WItem (w p v))
  selectBackground s _ = Tk_SelectBackground s
 
  selectForeground  :: String -> Conf (WItem (w p v))
  selectForeground s _ = Tk_SelectForeground s
 
  selectBorderwidth :: Int -> Conf (WItem (w p v))
  selectBorderwidth i _ = Tk_SelectBorderwidth i
 
-- Class HasText ---------------------------------------
 
class HasForeground w => HasText w where
  text :: String -> Conf w
  text s _ = Tk_Text s

  bitmap :: String -> Conf w
  bitmap s _ = Tk_Bitmap s

  underline :: Int -> Conf w
  underline i _ = Tk_Underline i

-- Class HasCommand ------------------------------------
 
class Widget w => HasCommand w where
  command :: GUI () -> Conf w
  command c _ = Tk_Command c
 
  active :: Bool -> Conf w
  active b _ = Tk_Active b
 
  activeBackground :: String -> Conf w
  activeBackground s _ = Tk_ActiveBackground s
 
  activeForeground :: String -> Conf w
  activeForeground s _ = Tk_ActiveForeground s

  invoke :: w -> GUI ()
  invoke w = cget w active ==> tk_toTcl w ["invoke"]

  destroyCommand ::  w -> GUI ()
  destroyCommand w = do
    s <- tk_getTcl ["deleteCommand",tk_getPathName w] 
    tk_delEventId s

-- Class GUIValue --------------------------------------

class (Text g) => GUIValue g where
  tk_convert         :: String -> OkOrErr g
  tk_defaultValue    :: g

  tk_toGUI           :: g -> String
  tk_toGUI           = show

  tk_fromGUI         :: String -> GUI g
  tk_fromGUI s       = okOrErr (tk_convert s)
    where
      okOrErr (Tk_Ok x)    = result x
      okOrErr (Tk_Err msg) =
        do {tk_showError msg; result tk_defaultValue}


--------------------------------------------------------------------------
-- The Kinds

-- TItem ---------------------------------------------------

data TItem a = TItem a String

instance TkWidget (TItem a) where
  tk_getPathName (TItem _ n) = n

instance Widget (TItem a) where
  cset w c =
    do let cs = c w             -- get option
       cs'  <- tk_showConf (tk_getPathName w) cs -- get option string
       let cs''      = words (tail cs')
           wm_option = ["wm", head cs'', tk_getPathName w] ++ tail cs''
       case cs of
           Tk_Title _        -> tk_putTcl wm_option
           Tk_WinPosition _  -> tk_putTcl wm_option
           Tk_WinSize _      -> tk_putTcl wm_option
           _                 -> tk_toTcl w ["configure",cs']
 
  cget w f =
    do let cs = f tk_defaultValue w
       cs'  <- tk_showConf (tk_getPathName w) cs
       let cs''      = head (words cs')
           wm_option = ["wm",tk_getPathName w , tail cs'']
       v <- case cs of
                Tk_Title s  -> tk_getTcl wm_option
                _           -> tk_fromTcl w ["cget",cs'']
       tk_fromGUI v

  onArgs e args a w = Tk_FreeOption act where
    act = do id <- tk_addCallBack (tk_getPathName w) a
             tk_putTcl [ "bind", tk_getPathName w, show e
                       , "{doEvent \"", show id
                       , concat (map (\x -> " %"++[x]) args), "\"}"
                       ]

  destroyEvent w e = done
  destroyEvents w = done

-- WItem ---------------------------------------------------
 
data WItem a = WItem a TkWidgetTree
 
tk_makeWItem :: (TkWidget (TItem t), Widget (WItem a))
    => [Config] -> a -> TItem t -> [Conf (WItem a)] -> String -> GUI (WItem a)
tk_makeWItem defaults wid win cs k =
  do x <- tk_newPathName
     let e = WItem wid (Tk_Node (tk_getPathName win, x, [], []) []) in
        do tk_putTcl [k, tk_getPathName e]
           csets e (map const defaults ++ cs)
           result e
 
instance TkWidget (WItem a) where
  tk_getPathName (WItem _ n) = tk_getNodeId n
 
instance Widget (WItem a) where
  cset w c =
    do cs <- tk_showConf (tk_getPathName w) (c w)
       if cs == "" then done else tk_toTcl w ["conf", cs]
 
  cget w f =
    do cs <- tk_showConf (tk_getPathName w) (f tk_defaultValue w)
       v  <- tk_fromTcl w ["cget", head (words cs)]
       tk_fromGUI v

  onArgs e args a w = Tk_FreeOption act where
    act = do id <- tk_addCallBack (tk_getPathName w) a
             tk_putTcl [ "bind", tk_getPathName w, show e
                       , "{doEvent \"", show id
                       , concat (map (\x -> " %"++[x]) args), "\"}"
                       ]

  destroyEvent w e = do
    s <- tk_getTcl ["deleteEvent",tk_getPathName w,show e]
    tk_delEventId s 

  destroyEvents w = do
    let x = tk_getPathName w
    tk_delEvents x
    tk_putTcl ["deleteEvents", x]

-- MItem ---------------------------------------------------
 
data MItem a = MItem a String Int
 
tk_makeMItem :: (TkWidget (TItem t), Widget (MItem a))
             => [Config] -> a -> TItem t -> [Conf (MItem a)] -> String -> GUI
             (MItem a)
tk_makeMItem defaults item men cs k =
  do let n = tk_getPathName men
     x <- tk_getTcl [ "addmenu ", n, "{", n, "add ", k, "}"]
     let e = MItem item n (numval x) in
       do csets e (map const defaults ++ cs)
          result e
 
instance TkWidget (MItem a) where
  tk_getPathName (MItem _ n _) = n
 
  tk_getVarName (MItem _ n i) = n ++ "i" ++ show i
 
instance Widget (MItem a) where
  cset (MItem m n i) c =
    do cs <- tk_showMenuConf n (c (MItem m n i))
       tk_putTcl [n, "entryconfigure", show i, cs]
 
  cget (MItem m n i) f =
    do cs <- tk_showMenuConf n (f tk_defaultValue (MItem m n i))
       v  <- tk_getTcl [n, "entrycget", show i, head (words cs)]
       tk_fromGUI v
 
  onArgs e args a w = Tk_FreeOption done
 
  destroyEvent w e = done
  destroyEvents w  = done

-- CItem ---------------------------------------------------

data Canvas0 = Canvas0
type Canvas  = WItem Canvas0

data CItem a = CItem a TkWidId Int
 
tk_makeCItem :: Widget (CItem c)
             => c -> Canvas -> [Int] -> [Conf (CItem c)] -> String -> GUI (CItem
             c)
tk_makeCItem i c ps cs k =
  do let n = tk_getPathName c
     x <- tk_getTcl ([n, "create", k] ++ map show ps)
     let e = CItem i n (numval x) in
       do csets e cs
          result e

instance TkWidget (CItem a) where
  tk_getPathName (CItem _ n _) = n
 
instance Widget (CItem a) where
  cset (CItem m n i) c =
    do cs <- tk_showConf n (c (CItem m n i))
       tk_putTcl [n, "itemconfigure", show i, cs]
 
  cget (CItem m n i) f =
    do cs <- tk_showConf n (f tk_defaultValue (CItem m n i))
       v <- tk_getTcl [n, "itemcget", show i, head (words cs)]
       tk_fromGUI v 
 
  onArgs e args a (CItem _ n i) = Tk_FreeOption act where
    act = do id <- tk_addCallBack (n++".@"++show i) a
             tk_putTcl [ n, "bind",show i, show e
                       , " { doEvent \"", show id
                       , concat (map (\x -> " %"++[x]) args), "\"}"
                       ]

  destroyEvent (CItem a n i) e = do
    s <- tk_getTcl ["deleteCanvasEvent",n,show i,show e]
    tk_delEventId s 

  destroyEvents (CItem a n i) = do
    tk_delThese (n++".@"++show i)
    tk_putTcl ["deleteCanvasEvents",n,show i]
      where name = (n++".@"++show i)++"."
            match n = name==n

instance Widget (CItem a) => HasCoords (CItem a) where
  moveObject (CItem _ ctop i) (x,y) =
    tk_putTcl [ctop, "move",show i,show x,show y]
 
  removeObject w@(CItem a ctop i) = do
    tk_delEvents ctop
    tk_putTcl [ctop, "delete",show i]
 
  lowerObject(CItem _ ctop i) =
    tk_putTcl [ctop,"lower", show  i]
 
  raiseObject(CItem _ ctop i) =
    tk_putTcl [ctop,"raise", show  i]
 
  getCoords (CItem _ ctop i) =
    do s <- tk_getTcl [ctop, "coords",show i]
       let ps = map (numval . takeWhile (/='.')) (words s)
       result (makeCoords ps)
    where makeCoords [] = []
          makeCoords (x:y:ss) = (x,y):makeCoords ss
 
  setCoords (CItem _ ctop i) ll =
    tk_putTcl ([ctop, "coords",show i]
       ++ (map (\(x,y) -> (show x)++" "++(show y)) ll))
 
--------------------------------------------------------------------------
-- Events

data TkEvent = Tk_Event String
 
instance Text TkEvent where
  showsPrec d (Tk_Event s) = showString s

-- Simple key events
 
key :: String -> TkEvent
key      s = Tk_Event ("<KeyPress-" ++ s ++ ">")
shiftkey s = Tk_Event ("<Shift-KeyPress-" ++ s ++ ">")

return, cursorUp, cursorDown, cursorLeft, cursorRight :: TkEvent
return      = key "Return"
cursorUp    = key "Up"
cursorDown  = key "Down"
cursorLeft  = key "Left"
cursorRight = key "Right"
 
-- Simple mouse events
 
click, doubleClick, motion :: Int -> TkEvent
click       i = Tk_Event ("<ButtonPress-"++show i ++ ">")
doubleClick i = Tk_Event ("<Double-ButtonPress-"++show i ++ ">")
motion      i = Tk_Event ("<B" ++ show i ++ "-Motion>")
  
mouseMotion :: TkEvent
mouseMotion = Tk_Event "<Motion>"

enterWidget, leaveWidget, destroyWidget :: TkEvent
enterWidget   = Tk_Event "<Enter>"
leaveWidget   = Tk_Event "<Leave>"
destroyWidget = Tk_Event "<Destroy>"

--------------------------------------------------------------------------
-- GUIValue

instance GUIValue a => GUIValue [a]
 
instance GUIValue String where
  tk_defaultValue    = ""
  tk_convert s       = Tk_Ok s
  tk_toGUI s         = s 
 
instance GUIValue Int where
  tk_defaultValue    = 0
  tk_convert (' ':s) = tk_convert s
  tk_convert ('+':s) = tk_convert s
  tk_convert ('-':s) = let i = tk_convert s
                    in case i of (Tk_Ok a)  -> Tk_Ok (-a)
                                 _          -> i
  tk_convert s | all (flip elem "0123456789") s = Tk_Ok (numval s)
  tk_convert s | otherwise = Tk_Err ("Invalid Int: " ++ show s)
  
instance GUIValue Bool where
  tk_defaultValue    = False
  tk_convert b       = tk_convert' (map toLower b)
    where -- standard representations -----------
 
          tk_convert' "true"  = Tk_Ok True
          tk_convert' "false" = Tk_Ok False
 
          tk_convert' "0"     = Tk_Ok False
          tk_convert' "1"     = Tk_Ok True
          
          -- representation for Active ----------
 
          tk_convert' "active"   = Tk_Ok True
          tk_convert' "normal"   = Tk_Ok True
          tk_convert' "disabled" = Tk_Ok False
 
          -- representation for Tk_Wrap -----------
 
          tk_convert' "word"     = Tk_Ok True
          tk_convert' "none"     = Tk_Ok False
 
          -- representation for Tk_MultipleSelect -----
 
          tk_convert' "extended" = Tk_Ok True
          tk_convert' "browse"   = Tk_Ok False
 
          -- representation for Tk_HorOrient --------
 
          tk_convert' "horizontal" = Tk_Ok True
          tk_convert' "vertcial"   = Tk_Ok False
 
          -- error case ------------------------
 
          tk_convert' s = Tk_Err ("Invalid Bool: " ++ show s)

instance GUIValue (Int,Int) where
  tk_defaultValue = (0,0) 

  -- representation for text positions --
  tk_toGUI (a,b)  = show a ++ "." ++ show b
  tk_convert xs   = Tk_Ok (numval a, numval (drop 1 b))
                    where (a,b) = span (/= '.') xs
 
instance GUIValue [(Int,Int)] where
  tk_defaultValue = []

  -- representation for text positions --

  tk_toGUI xs     = unwords (map tk_toGUI xs)
  tk_convert xs   = let ys = map tk_convert (words xs)
                    in Tk_Ok (map (\(Tk_Ok x) -> x) ys)


data OkOrErr a    = Tk_Ok a | Tk_Err String
 
tk_showError     :: String -> GUI ()
tk_showError s   = tk_putTcl ["tk_dialog .tkerr {Input Error}",
                               tk_secure s, "error 0 OK"
                             ]

--------------------------------------------------------------------------
-- TkWidgetTree

type TkTop   = String      -- window name
type TkChild = String      -- windowitem name

data TkWidgetTree
  = Tk_Node (TkTop, TkChild, [Config], [TkLPack]) [TkWidgetTree]

tk_emptyNode :: TkWidgetTree
tk_emptyNode = Tk_Node ("", "", [], []) []

tk_getNodeId :: TkWidgetTree -> TkWidId
tk_getNodeId (Tk_Node (n,m,_,_) _) = n++m

tk_getTop :: TkWidgetTree -> TkTop
tk_getTop (Tk_Node (n,_,_,_) _) = n

tk_getChild :: TkWidgetTree -> TkChild
tk_getChild (Tk_Node (_,n,_,_) _) = n

--------------------------------------------------------------------------
-- Packing

-- TkPack

data TkPack = Tk_Side String
            | Tk_Fill String
            | Tk_Expand
            | Tk_Group

instance Eq TkPack where
  Tk_Side x == Tk_Side y = x == y
  Tk_Fill x == Tk_Fill y = x == y
  Tk_Expand == Tk_Expand = True
  Tk_Group  == Tk_Group  = True
  _      == _      = False


instance Text TkPack where
  showsPrec d (Tk_Side s)   = showString "-si " . showString s
  showsPrec d (Tk_Fill s)   = showString "-fi " . showString s
  showsPrec d Tk_Group      = showString ""
  showsPrec d Tk_Expand     = showString "-ex 1"
 
-- TkLPack

type TkLPack = TkTag TkPack
data TkTag a = Tk_Pack1 a
             | Tk_Pack2 a

instance Eq TkLPack where
  Tk_Pack1 a == Tk_Pack1 b = a == b
  Tk_Pack2 a == Tk_Pack2 b = a == b
  _          == _          = False


-- Lifting Widgets

{-  already defined in other module
data Frame0 = Frame0
type Frame  = WItem Frame0
-}

tk_WidgetToFrame :: Widget (WItem w) => WItem w -> Frame
tk_WidgetToFrame (WItem w ts) = WItem Frame0 ts

tk_combine :: (Widget (WItem a), Widget (WItem b))
            => [TkPack] -> WItem a -> WItem b -> Frame
tk_combine p a b =
  let (WItem Frame0 a') = tk_WidgetToFrame a
      (WItem Frame0 b') = tk_WidgetToFrame b
  in WItem Frame0 (tk_combine' p a' b')
  where
    tk_combine' s a b =
      Tk_Node (tk_getTop a, tk_getChild a ++ "f", [], [])
               (lift a s ++ lift b s)
      where
        lift (Tk_Node (n,m,cs,[]) (w:ws)) s
          | all (\(Tk_Node (n,m,cs,ps) w)
                   -> s == [a | Tk_Pack2 a <- ps]) (w:ws) = (w:ws)
        lift (Tk_Node (n,m,cs,ps)  ws) s
          | otherwise = [Tk_Node (n,m,cs,nub (ps ++ map Tk_Pack2 s)) ws]


tk_add :: Widget (WItem a) => [TkPack] -> WItem a -> WItem a 
tk_add p (WItem a (Tk_Node (n, m, cs, ps) ws)) =
  WItem a (Tk_Node (n, m, cs, nub (ps ++ map Tk_Pack1 p)) ws)

-- Algorithm
 
tk_packIn :: TkWidId -> [Config] -> TkWidgetTree -> GUI ()
 
tk_packIn father _ (Tk_Node (top, son, configs, packinfo) []) =
  do let info' = map show (tk_packUnion packinfo)
     tk_putTcl (["praise", top++son, father] ++ info')
     cs'      <- binds (map (tk_showConf (top++son)) configs)
     if cs' == [] then done else tk_putTcl ([top++son, "configure "] ++ cs')

tk_packIn father cs (Tk_Node (top, son, configs, packinfo) children) =
  do cs'  <- binds (map (tk_showConf (top++son)) (configs ++ cs))
     let info' = map show (tk_packUnion packinfo)
     tk_putTcl (["frame ", top++son] ++ cs')
     tk_putTcl (["praise", top++son , father] ++ info')
     seqs (map (tk_packIn (top++son) cs) children)
 
tk_packUnion :: [TkLPack] -> [TkPack]
tk_packUnion ps =
 let ps' = nub ([p | Tk_Pack1 p <- ps] ++ [p | Tk_Pack2 p <- ps])
 in  if (Tk_Fill "x" `elem` ps' && Tk_Fill "y" `elem` ps') ||
        Tk_Fill "both" `elem` ps'
     then ps' ++ [Tk_Fill "both"]
     else ps'

--------------------------------------------------------------------------
-- Layout Combinators

infixl 7 <<, <*<, <-<, <|<, <+<, <*-<, <*|<, <*+<
 
(<<),(<*<),(<-<),(<|<),(<+<),(<*-<),(<*|<),(<*+<) ::
   (Widget (WItem a),Widget (WItem b)) => WItem a -> WItem b -> Frame
 
(<<)   = tk_combine [Tk_Side "left"]
(<*<)  = tk_combine [Tk_Side "left", Tk_Expand]
(<-<)  = tk_combine [Tk_Side "left", Tk_Fill "x"]
(<|<)  = tk_combine [Tk_Side "left", Tk_Fill "y"]
(<+<)  = tk_combine [Tk_Side "left", Tk_Fill "both"]
(<*-<) = tk_combine [Tk_Side "left", Tk_Expand, Tk_Fill "x"]
(<*|<) = tk_combine [Tk_Side "left", Tk_Expand, Tk_Fill "y"]
(<*+<) = tk_combine [Tk_Side "left", Tk_Expand, Tk_Fill "both"]
 
infixl 6 ^^, ^*^, ^-^, ^|^, ^+^, ^*-^, ^*|^, ^*+^
 
(^*^),(^^),(^-^),(^|^),(^+^),(^*-^),(^*|^),(^*+^) ::
   (Widget (WItem a), Widget (WItem b)) => WItem a -> WItem b -> Frame
 
(^*^)  = tk_combine [Tk_Side "top", Tk_Expand]
(^^)   = tk_combine [Tk_Side "top"]
(^-^)  = tk_combine [Tk_Side "top", Tk_Fill "x"]
(^|^)  = tk_combine [Tk_Side "top", Tk_Fill "y"]
(^+^)  = tk_combine [Tk_Side "top", Tk_Fill "both"]
(^*-^) = tk_combine [Tk_Side "top", Tk_Expand, Tk_Fill "x"]
(^*|^) = tk_combine [Tk_Side "top", Tk_Expand, Tk_Fill "y"]
(^*+^) = tk_combine [Tk_Side "top", Tk_Expand, Tk_Fill "both"]

 
matrix :: Widget (WItem a) => Int -> [WItem a] -> Frame
matrix n ls = matrix' n (map tk_WidgetToFrame ls)

matrix' :: Int -> [Frame] -> Frame
matrix' n = foldl1 (^^) . map (foldl1 (<<)) . groupn n

horizontal, vertical :: Widget (WItem a) => [WItem a] -> Frame
horizontal xs = matrix (length xs) xs
vertical   xs = matrix 1 xs

-- Sizing

expand, fillX, fillY, fillXY,flexible :: Widget (WItem a) => WItem a -> WItem a
expand   = tk_add [Tk_Expand]
fillX    = tk_add [Tk_Fill "x"]
fillY    = tk_add [Tk_Fill "y"]
fillXY   = tk_add [Tk_Fill "both"]
flexible = fillXY . expand

--------------------------------------------------------------------------
-- Protecting Strings
 
tk_secure :: String -> String
tk_secure s = "\"" ++ secure s ++ "\""
  where
    secure [] = []
    secure (x:xs) = x' ++ secure xs where
      x' = case x of
             '\n' -> "\\n"
             '\t' -> "\\t"
             '['  -> "\\["
             ']'  -> "\\]"
             '{'  -> "\\{"
             '}'  -> "\\}"
             '\\' -> "\\\\"
             '\"' -> "\\\""
             '$'  -> "\\$"
             _    -> [x]

--------------------------------------------------------------------------
-- Configurations

-- RGB Colors

rgb :: Int -> Int -> Int -> String
rgb r g b = "#" ++ concat (map (hex 2 "") [r,g,b]) where
  hex 0 rs _ = rs
  hex t rs 0 = hex (t-1) ('0':rs) 0
  hex t rs i = let m = mod i 16
               in hex (t-1)((chr (48+m+7*(div m 10))):rs)(div i 16)

-- type Config

data Menu0 = Menu0 [Config]
type Menu  = TItem Menu0

type Conf w = w -> Config
 
data Config = Tk_Text String
            | Tk_Width Int
            | Tk_Height Int
            | Tk_PadX Int
            | Tk_PadY Int
            | Tk_TakeFocus Bool
            | Tk_IndicatorColor String
            | Tk_HighlightBackground String
            | Tk_HighlightColor String
            | Tk_HighlightThickness Int
            | Tk_SelectBackground String
            | Tk_SelectForeground String
            | Tk_SelectBorderwidth Int
            | Tk_Command (GUI ())
            | Tk_Title String
            | Tk_WinSize (Int,Int)           --- ## not yet readable
            | Tk_WinPosition (Int,Int)       --- ## not yet readable
            | Tk_IndicatorOn Bool
            | Tk_ActiveForeground String
            | Tk_ActiveBackground String
            | Tk_Background String
            | Tk_Foreground String
            | Tk_FillColor String
            | Tk_OutlineColor String
            | Tk_Bitmap String
            | Tk_Active Bool
            | Tk_Underline Int
            | Tk_ScaleText String
            | Tk_ScaleHeight Int
            | Tk_Cursor String
            | Tk_ScaleRange (Int,Int)        --- ## not yet readable
            | Tk_SliderLength Int
            | Tk_TickInterval Int
            | Tk_TroughColor String
            | Tk_Justify String
            | Tk_Relief String     -- raised/flat/ridge/sunken/groove
            | Tk_Aspect Int
            | Tk_Font String
            | Tk_Wrap Bool
            | Tk_Anchor String   -- "n","ne","e","se","s","sw","w","nw","c"
            | Tk_BorderWidth Int
            | Tk_MultipleSelect Bool
            | Tk_ScrollRegion (Int,Int)      --- ## not yet readable
            | Tk_Variable String      -- internal use only
            | Tk_HorOrient Bool       -- ""
            | Tk_TextVariable String  -- ""
            | Tk_OnValue  String      -- ""
            | Tk_OffValue String      -- ""
            | Tk_Value Int            -- ""
            | Tk_FreeOption  (GUI ())   -- ""
            | Tk_InitValue (GUI ())   -- ""
            | Tk_Pulldown Menu        -- ""
            | Tk_AngleExtent Int      -- size of the angular range of the arc
            | Tk_AngleStart Int       -- start angle for arc
            | Tk_DummyConf            -- ""
 

-- Show functions

instance Text Config where
  showsPrec d (Tk_Text s)
    = showString ("text " ++ tk_secure s)
  showsPrec d (Tk_Underline i)
    = showString "underline " . shows i
  showsPrec d (Tk_Title t)
    = showString ("title " ++ tk_secure t)
  showsPrec d (Tk_WinSize (x, y))
    = showString ("geometry " ++ show x ++ "x" ++ show y)
  showsPrec d (Tk_WinPosition (x, y))
    = showString ("geometry +" ++ show x ++ "+" ++ show y)
  showsPrec d (Tk_PadX i)
    = showString "padx " . shows i
  showsPrec d (Tk_PadY i)
    = showString "pady " . shows i
  showsPrec d (Tk_TakeFocus False)
    = showString "takefocus 0 "
  showsPrec d (Tk_TakeFocus True)
    = showString "takefocus 1 "
  showsPrec d (Tk_Height i)
    = showString "height " . shows i
  showsPrec d (Tk_Width i)
    = showString "width " . shows i
  showsPrec d (Tk_Command a)
    = showString "command "
  showsPrec d (Tk_InitValue a)
    = showString "initvalue "
  showsPrec d (Tk_SelectBackground c)
    = showString "selectbackground " . shows c
  showsPrec d (Tk_SelectForeground c)
    = showString "selectforeground " . shows c
  showsPrec d (Tk_SelectBorderwidth i)
    = showString "selectborderwidth " . shows i
  showsPrec d (Tk_Background c)
    = showString "background " . shows c
  showsPrec d (Tk_Foreground c)
    = showString "foreground " . shows c
  showsPrec d (Tk_HighlightBackground c)
    = showString "highlightbackground " . shows c
  showsPrec d (Tk_HighlightColor c)
    = showString "highlightcolor " . shows c
  showsPrec d (Tk_HighlightThickness i)
    = showString "highlightthickness " . shows i
  showsPrec d (Tk_Bitmap n)
    = showString ("bitmap @" ++ n)
  showsPrec d (Tk_Active True)
    = showString "state normal"
  showsPrec d (Tk_Active False)
    = showString "state disabled"
  showsPrec d (Tk_FillColor c)
    = showString "fill " . shows c
  showsPrec d (Tk_OutlineColor c)
    = showString "outline " . shows c
  showsPrec d (Tk_IndicatorColor c)
    = showString "selectcolor " . shows c
  showsPrec d (Tk_IndicatorOn True)
    = showString "indicatoron 1"
  showsPrec d (Tk_IndicatorOn False)
    = showString "indicatoron 0"
  showsPrec d (Tk_Justify s)
    = showString "justify " . shows s
  showsPrec d (Tk_Relief r)
    = showString ("relief " ++ tk_secure r)
  showsPrec d (Tk_Aspect i)
    = showString "aspect " . shows i
  showsPrec d (Tk_ScaleText s)
    = showString ("label " ++ tk_secure s)
  showsPrec d (Tk_ScaleHeight s)
    = showString "length " . shows s
  showsPrec d (Tk_Cursor s)
    = showString "cursor " . shows s
  showsPrec d (Tk_ActiveForeground c)
    = showString "activeforeground " . shows c
  showsPrec d (Tk_ActiveBackground c)
    = showString "activebackground " . shows c
  showsPrec d (Tk_HorOrient True)
    = showString "orient horizontal"
  showsPrec d (Tk_HorOrient False)
    = showString "orient vertical"
  showsPrec d (Tk_ScaleRange (i,j))
    = showString "from " . shows i . showString " -to " . shows j  -- ###
  showsPrec d (Tk_SliderLength i)
    = showString "sliderlength " . shows i
  showsPrec d (Tk_TickInterval i)
    = showString "tickinterval " . shows i
  showsPrec d (Tk_TroughColor c)
    = showString "troughcolor " . shows c
  showsPrec d (Tk_ScrollRegion (i,j))
    = showString ("scrollregion {0 0 " ++ show i ++ " " ++ show j ++ "}")
  showsPrec d (Tk_Font s)
    = showString "font " . shows s
  showsPrec d (Tk_Anchor s)
    = showString "anchor " . shows s
  showsPrec d (Tk_Wrap True)
    = showString "wrap word"
  showsPrec d (Tk_Wrap False)
    = showString "wrap none"
  showsPrec d (Tk_Variable s)
    = showString "variable " . shows s
  showsPrec d (Tk_TextVariable s)
    = showString "textvariable " . shows s
  showsPrec d (Tk_OnValue s)
    = showString "onvalue " . shows s
  showsPrec d (Tk_OffValue s)
    = showString "offvalue " . shows s
  showsPrec d (Tk_Value s)
    = showString "value " . shows s
  showsPrec d (Tk_BorderWidth i)
    = showString "borderwidth " . shows i
  showsPrec d (Tk_MultipleSelect True)
    = showString "selectmode extended"
  showsPrec d (Tk_MultipleSelect False)
    = showString "selectmode browse"
  showsPrec d (Tk_Pulldown m)
    = showString "menu " . shows (tk_getPathName m)
  showsPrec d (Tk_AngleExtent i)
    = showString "extent " . shows i
  showsPrec d (Tk_AngleStart i)
    = showString "start " . shows i
  showsPrec d (Tk_FreeOption a)
    = showString "dummy"

-- send them to Tcl

tk_showMenuConf :: String -> Config -> GUI String
tk_showMenuConf _ (Tk_Text s) = result ("-label " ++ (tk_secure s))
tk_showMenuConf pathname s    = tk_showConf pathname s
 
tk_showConf :: String -> Config -> GUI String
tk_showConf _ (Tk_DummyConf) = result ""
tk_showConf _ (Tk_InitValue f) = do {f; result ""}
tk_showConf _ (Tk_FreeOption f) = do {f; result ""}
tk_showConf pathname (Tk_Command f)   =
  do i <- tk_addCallBack pathname (\_ -> f)
     result ("-command {doEvent " ++ show i ++ "}")
tk_showConf _ c = result ('-' : show c)

--------------------------------------------------------------------------
-- Toplevel Widgets

-- Window ---------------------------------------------

data Window0 = Window0 [Config]
type Window  = TItem Window0
 
tk_getWinDef :: Window -> [Config]
tk_getWinDef (TItem (Window0 cs) n) = cs
 
instance HasHeight     Window
instance HasWidth      Window
instance HasFocus      Window
instance HasBorder     Window
instance HasBackground Window
 
window :: [Conf Window] -> GUI Window
window cs =
  do x <- tk_newPathName
     let w = (TItem (Window0 []) x) in
       do tk_putTcl ["window", x]
          id <- tk_addCallBack x (\_ -> destroy w)
          tk_putTcl [ "wm protocol", x,"WM_DELETE_WINDOW"
                    , "{doEvent \"", show id , "\"}"
                    ]
          csets w cs
          result w
 
windowDefault :: [Conf Window] -> [Conf Default] -> GUI Window
windowDefault ct cf =
   do (TItem _ n) <- window ct
      result (TItem (Window0 [c (WItem Tk_Default tk_emptyNode) | c <- cf]) n)
 
closeWindow :: Window -> GUI ()
closeWindow = destroy

openWindow :: Widget (WItem w) 
           => [Conf Window] -> (Window -> GUI (WItem w)) -> GUI ()
openWindow cs f = openDefault cs [] f
 
openDefault :: Widget (WItem w) 
            => [Conf Window] -> [Conf Default] 
               -> (Window -> GUI (WItem w)) -> GUI ()
openDefault cs ds f =
   do v <- windowDefault cs ds
      w <- f v
      packDefault w ds
 
pack :: Widget (WItem a) => WItem a -> GUI ()
pack t = packDefault t []
 
packDefault :: Widget (WItem a) => WItem a -> [Conf Default] -> GUI ()
packDefault w cs = let (WItem _ wt) = flexible w in
  tk_packIn (tk_getTop wt) defaults wt
    where
     defaults =
      map tk_noForeground [c (WItem Tk_Default tk_emptyNode) | c <- cs]

title :: String -> Conf Window
title s a = Tk_Title s
 
winSize     :: (Int,Int) -> Conf Window
winSize (x,y) w = Tk_WinSize (x,y)
 
winPosition :: (Int,Int) -> Conf Window
winPosition (x,y) w = Tk_WinPosition (x,y)
 
--------------------------------------------------------------------------
-- Window Items

-- Default ---------------------------------------------
 
data TkDefault = Tk_Default
type Default = WItem TkDefault

instance HasForeground Default
instance HasBackground Default
 
tk_noForeground :: Config -> Config
tk_noForeground x =
 case x of (Tk_Foreground _) -> Tk_DummyConf
           (Tk_Font _)       -> Tk_DummyConf
           _                 -> x

-- Frame ---------------------------------------------
 
data Frame0 = Frame0
type Frame  = WItem Frame0
 
instance HasHeight     Frame
instance HasBorder     Frame
instance HasFocus      Frame
instance HasWidth      Frame
instance HasBackground Frame
 
frame :: Widget (WItem a) => [Conf Frame] -> WItem a -> GUI Frame
frame cs w =
  let (WItem Frame0 node) = tk_WidgetToFrame (flexible w)
      cs1 = map (\f -> f (WItem Frame0 tk_emptyNode)) cs
      cs2 = map tk_noForeground cs1
      m   = tk_getTop node
  in do n <- tk_newPathName
        result (WItem Frame0 (Tk_Node (m, n, cs2,[]) [node]))

-- Label -----------------------------------------------
 
data Label0 = Label0
type Label  = WItem Label0
 
instance HasAnchor     Label
instance HasBorder     Label
instance HasFocus      Label
instance HasWidth      Label
instance HasHeight     Label
instance HasPad        Label
instance HasBackground Label
instance HasForeground Label
instance HasText       Label
 
label :: [Conf Label] -> Window -> GUI Label
label cs t = tk_makeWItem defaults Label0 t cs "label"
  where defaults = tk_getWinDef t
 

-- Scrollbar -------------------------------------------
 
data Scrollbar0 = Scrollbar0
type Scrollbar  = WItem Scrollbar0
 
instance HasWidth      Scrollbar
instance HasFocus      Scrollbar
instance HasBorder     Scrollbar
instance HasBackground Scrollbar
 
scrollbar :: [Conf Scrollbar] -> Window -> GUI Scrollbar
scrollbar cs t = tk_makeWItem defaults Scrollbar0 t cs "scrollbar"
   where defaults = map tk_noForeground (tk_getWinDef t)

hscroll, vscroll :: HasScroll (WItem w)
                 => [Conf Scrollbar] -> WItem w -> GUI Scrollbar
vscroll cs w@(WItem _ wt) =
 do v <- scrollbar cs (TItem (Window0 []) (tk_getTop wt))
    tk_toTcl w [ "configure -yscrollcommand {", tk_getPathName v, "set}"]
    tk_toTcl v [ "configure -command {", tk_getPathName w, "yview}"]
    result v

hscroll cs w@(WItem _ wt) =
  do v <- scrollbar (const (Tk_HorOrient True):cs)
                    (TItem (Window0 []) (tk_getTop wt))
     tk_toTcl w [ "configure -xscrollcommand {", tk_getPathName v, "set}"]
     tk_toTcl v [ "configure -command {", tk_getPathName w, "xview}"]
     result v

-- Scale -----------------------------------------------

data Scale0 a = Scale0 a
type Scale = WItem (Scale0 Int)

instance HasBackground Scale
instance HasForeground Scale
instance HasFocus      Scale
instance HasBorder     Scale 
instance HasWidth      Scale 
instance HasCommand    Scale where
  activeBackground _ _ = Tk_DummyConf
instance HasText       Scale where
  text s w      = Tk_ScaleText s
  underline _ _ = Tk_DummyConf
  bitmap    _ _ = Tk_DummyConf
instance HasHeight     Scale where
  height s w = Tk_ScaleHeight s
instance GUIValue v => HasInput WItem Scale0 v where
  getValue w =
    do s <- tk_fromTcl w ["get"]
       tk_fromGUI s

  setValue w i =
    tk_output (tk_toGUI i) $ \x ->
    tk_toTcl w ["set", x]

vscale, hscale :: [Conf Scale] -> Window -> GUI Scale
vscale cs t = tk_makeWItem defaults (Scale0 0) t cs "scale"
  where defaults = tk_getWinDef t

hscale cs t = 
  tk_makeWItem defaults (Scale0 0) t (const (Tk_HorOrient True):cs) "scale"
  where defaults = tk_getWinDef t

scaleRange :: (Int,Int) -> Conf Scale
scaleRange (x,y) w = Tk_ScaleRange (x,y)

sliderLength     :: Int    -> Conf Scale
sliderLength i w = Tk_SliderLength i

tickInterval     :: Int    -> Conf Scale
tickInterval i w = Tk_TickInterval i

troughColor :: String -> Conf Scale
troughColor s w = Tk_TroughColor s


-- Entry -----------------------------------------------

data Entry0 a = Entry0 a
type Entry  a = WItem (Entry0 a)

instance HasBackground (Entry a)
instance HasForeground (Entry a)
instance HasFocus      (Entry a)
instance HasBorder     (Entry a)
instance HasWidth      (Entry a)
instance HasScroll     (Entry a)

instance GUIValue v => HasInput WItem Entry0 v where
  getValue w  =
    do s <- tk_fromTcl w ["get"]
       tk_fromGUI s
 
  setValue w s =
    tk_output (tk_toGUI s) $ \x ->
    tk_putTcl [ "global",tk_getPathName w
              , ";set",tk_getPathName w, x
              ]
 
entry :: (Widget (Entry a), GUIValue a) 
      => [Conf (Entry a)] -> Window -> GUI (Entry a)
entry cs w =
  do e <- tk_makeWItem defaults (Entry0 tk_defaultValue) w [] "entry"
     let c = const (Tk_TextVariable (tk_getPathName e)) in csets e (c:cs)
     result e
  where defaults = tk_getWinDef w


-- Edit ------------------------------------------------
 
data Edit0 p a = Edit0 p a
type Edit = WItem (Edit0 (Int,Int) String)

instance HasBackground Edit 
instance HasForeground Edit
instance HasWidth      Edit
instance HasBorder     Edit
instance HasFocus      Edit
instance HasHeight     Edit
instance HasPad        Edit
instance HasScroll     Edit
 
instance HasInput WItem (Edit0 (Int,Int)) String where
  getValue w    = do { x <- tk_fromTcl w ["get","1.0","end"]; result x }
  setValue w ls = do { tk_toTcl w ["delete","1.0","end"]; putEnd w ls }
 
instance HasPosition Edit0 (Int,Int) String where
 
  getFromTo w p1 p2 = do {y <- tk_fromTcl w ["get", tk_toGUI p1, tk_toGUI p2];
  tk_fromGUI y}
 
  putEnd  w s =
    tk_output (tk_toGUI s) $ \x ->
    tk_toTcl w ["insert","end", x]
 
  putBegin w s = putPos w (1,0) s
 
  putPos w p s =
    tk_output (tk_toGUI s) $ \x ->
    tk_toTcl w ["insert", tk_toGUI p, x]
 
  setSelection w p = done
 
  getSelection w   =
    do s <- tk_fromTcl w ["tag","ranges","sel"]
       case s of 
            ""  -> result []
            _   -> do let (a:b:ps) = map tk_str2pos (words s ++ repeat "")
                      result [a,b]
    where
      tk_str2pos xs = (numval a, numval (tail b))
         where (a,b) = span (/= '.') xs
 
  getSize w = do { s <- tk_fromTcl w ["index end"]; tk_fromGUI s}
 
edit :: [Conf Edit] -> Window -> GUI Edit
edit cs t = tk_makeWItem defaults (Edit0 (0,0) "") t cs "text"
  where defaults = tk_getWinDef t
 
wrap :: Bool -> Conf Edit
wrap b w = Tk_Wrap b


-- Mark ------------------------------------------------
 
data Mark = Mark String
 
mouseMark, insMark, endMark :: Mark
mouseMark = Mark "current"
insMark   = Mark "insert"
endMark   = Mark "end"
 
setMark :: Edit -> (Int,Int) -> GUI Mark
setMark w p =
  do x <- tk_newPathName
     tk_toTcl w ["mark set", "m" ++ x, tk_toGUI p]
     result (Mark ("m" ++ x))
 
getMark :: Edit -> Mark -> GUI (Int,Int)
getMark w (Mark x) =
  do {s <- tk_fromTcl w ["index", x]; tk_fromGUI s}


-- Tag -------------------------------------------------
 
type TkTagId = String
 
data Tag0 a  = Tag TkWidId TkTagId
type Tag = Tag0 ()
 
instance Widget (Tag0 ()) where
  cset (Tag n i) c =
    do cs <- tk_showConf (n ++ ".@" ++ show i) (c (Tag n i))
       tk_putTcl [n, "tag configure", i, cs]
 
  cget (Tag n i) f =
    do cs <- tk_showConf (n ++ ".@" ++ show i) (f tk_defaultValue (Tag n i))
       v  <- tk_getTcl [n, "tag cget", i, cs]
       tk_fromGUI v
 
  onArgs e args a (Tag n i) = Tk_FreeOption act where
    act = do id <- tk_addCallBack (n ++ ".@" ++ show i) a
             tk_putTcl [ n, "tag bind",show i, show e
                       , " { doEvent \"", show id
                       , concat (map (\x -> " %"++[x]) args), "\"}"
                       ]

instance TkWidget Tag 
instance HasBackground Tag
instance HasForeground Tag
instance HasText       Tag
 
tag :: [(Int,Int)] -> [Conf Tag] -> Edit -> GUI Tag
tag ps cs w =
  do x <- tk_newPathName
     tk_toTcl w (["tag add", ("t" ++ x), tk_toGUI ps])
     let t = Tag (tk_getPathName w) ("t" ++ x) in do {csets t cs ; result t}
 
putPosTag :: Edit -> (Int,Int) -> String -> [Conf Tag] -> GUI Tag
putPosTag w p1 s cs =
  do m  <- setMark w p1
     putPos w p1 s
     p2 <- getMark w m
     tag [p1,p2] cs w

putEndTag :: Edit -> String -> [Conf Tag] -> GUI Tag
putEndTag w s cs =
  do let n = tk_getPathName w
     t <- tk_newPathName
     tk_output (tk_toGUI s) $ \x -> do
       tk_putTcl ["insTag", n, x, "t"++ t]
       let v = Tag n ("t" ++ t) in do {csets v cs ; result v}

delTag :: Tag -> GUI ()
delTag (Tag n i) = tk_putTcl [n, "tag delete", i]
 
tagRange :: Tag -> GUI [(Int,Int)]
tagRange (Tag n i) = do ps <- tk_getTcl [n, "tag ranges", i]
                        tk_fromGUI ps
 
lowerTag :: Tag -> GUI ()
lowerTag (Tag n i) = tk_putTcl [n, "tag lower", i]

-- Listbox ---------------------------------------------

data Listbox0 p a = Listbox0 p a
type Listbox a = WItem (Listbox0 Int a)

instance HasBackground (Listbox v) 
instance HasForeground (Listbox v)
instance HasFocus      (Listbox v)
instance HasScroll     (Listbox v)
instance HasBorder     (Listbox v)
instance HasWidth      (Listbox v)
instance HasHeight     (Listbox v)
 
instance HasPosition Listbox0 Int [v]
  => HasInput WItem (Listbox0 Int) [v] where
 
  setValue w ls = do { tk_toTcl w ["delete","0","end"]; putEnd w ls }
 
  getValue w    = do { x <- getSize w ; getFromTo w 0 (x-1) }
  
  readOnly _ _ = Tk_DummyConf
 
instance GUIValue v => HasPosition Listbox0 Int [v] where
 
  getFromTo w p1 p2 =
     binds [do {y <- tk_fromTcl w ["get", tk_toGUI p]; tk_fromGUI y}
              | p <- [p1..p2]
              ]
 
  putEnd w ls =
    seqs [  tk_output (tk_toGUI y) $ \x ->
            tk_toTcl w ["insert","end", x]
         | y <- ls ]
 
  putBegin w ls = putPos w 0 ls
 
  putPos w p ls =
    seqs [  tk_output (tk_toGUI y) $ \x ->
            tk_toTcl w ["insert" ,tk_toGUI p, x]
         | y <- reverse ls]
 
  setSelection w ps =
    do tk_toTcl w ["sel","clear 0 end"]
       seqs (map (\p -> tk_toTcl w ["sel","set",tk_toGUI p,tk_toGUI p]) ps)
 
  getSelection w =
    do xs <- tk_fromTcl w ["curselection"]
       result (map numval (words xs))
 
  getSize w = do {s <- tk_fromTcl w ["size"] ; tk_fromGUI s }
 
listbox :: (Widget (Listbox a), GUIValue a) =>
         [Conf (Listbox a)] -> Window -> GUI (Listbox a)
listbox cs t = tk_makeWItem defaults (Listbox0 0 tk_defaultValue) t cs "listbox"
 where defaults = tk_getWinDef t
 
multipleSelect :: Bool -> Conf (Listbox a)
multipleSelect b w = Tk_MultipleSelect b


-- Message0 ---------------------------------------------
 
data Message0 = Message0
type Message  = WItem Message0
 
instance HasAnchor     Message
instance HasWidth      Message
instance HasFocus      Message
instance HasBorder     Message
instance HasBackground Message
instance HasForeground Message
instance HasPad        Message
instance HasText       Message where
  underline _ _ = Tk_DummyConf
  bitmap    _ _ = Tk_DummyConf
 
message :: [Conf Message] -> Window -> GUI Message
message cs t = tk_makeWItem defaults Message0 t cs "message"
  where defaults = tk_getWinDef t
 
aspect :: Int -> Conf Message
aspect i w = Tk_Aspect i
 
--------------------------------------------------------------------------
-- Menu's and Buttons

-- Menu
 
{- already defined
data Menu0 = Menu0 [Config]
type Menu  = TItem Menu0
-}

instance HasBorder     Menu
instance HasBackground Menu
 
tk_getMenDef :: Menu -> [Config]
tk_getMenDef (TItem (Menu0 cs) n) = cs
 
menu :: Widget (c Menubutton0)
     => [Conf Menu] -> c Menubutton0 -> GUI Menu
menu cs b =
  do x <- tk_newPathName
     let e = (TItem (Menu0 []) (tk_getPathName b ++ x)) in
        do tk_putTcl ["menu0", tk_getPathName e]
           cset b (\_ -> Tk_Pulldown e)
           csets e cs
           result e
 
menuDefault :: Widget (c Menubutton0)
            => [Conf Menu] -> [Conf Default] -> c Menubutton0 -> GUI Menu
menuDefault ct cf b =
    do (TItem _ n) <- menu ct b
       result (TItem (Menu0 [c (WItem Tk_Default tk_emptyNode) | c <- cf]) n)
 
popup :: (Int, Int) -> Menu -> GUI ()
popup (x,y) m = tk_toTcl m ["post", show x, show y]


-- Button ----------------------------------------------
 
data Button0 = Button0
type Button  = WItem Button0
type MButton = MItem Button0

instance HasBackground Button 
instance HasForeground Button
instance HasFocus      Button
instance HasAnchor     Button
instance HasBorder     Button
instance HasWidth      Button
instance HasHeight     Button
instance HasPad        Button
instance HasText       Button
instance HasCommand    Button
instance HasBackground MButton 
instance HasForeground MButton
instance HasText       MButton
instance HasCommand    MButton
 
button :: [Conf Button] -> Window -> GUI Button
button cs t = tk_makeWItem defaults Button0 t cs "button"
  where defaults = tk_getWinDef t

mbutton :: [Conf MButton] -> Menu -> GUI MButton
mbutton cs t = tk_makeMItem defaults Button0 t cs "command"
  where defaults = tk_getMenDef t

-- Radiobutton0  ----------------------------------------------
 
data Radiobutton0 = Radiobutton0
type Radiobutton  = WItem Radiobutton0
type MRadiobutton = MItem Radiobutton0
 
instance HasAnchor     Radiobutton
instance HasHeight     Radiobutton
instance HasWidth      Radiobutton
instance HasFocus      Radiobutton
instance HasBorder     Radiobutton
instance HasPad        Radiobutton
instance HasBackground Radiobutton 
instance HasForeground Radiobutton
instance HasText       Radiobutton
instance HasCommand    Radiobutton
instance HasIndicator  Radiobutton
instance HasBackground MRadiobutton 
instance HasForeground MRadiobutton
instance HasText       MRadiobutton
instance HasCommand    MRadiobutton
instance HasIndicator  MRadiobutton
 
radiobutton :: [Conf Radiobutton] -> Window -> GUI Radiobutton
radiobutton cs t = tk_makeWItem defaults Radiobutton0 t cs "radiobutton"
  where defaults = tk_getWinDef t
 
mradiobutton :: [Conf MRadiobutton] -> Menu -> GUI MRadiobutton
mradiobutton cs t = tk_makeMItem defaults Radiobutton0 t cs "radiobutton"
  where defaults = tk_getMenDef t


-- Radio -----------------------------------------------

data Radio0 v = Radio0 v [Config]
type Radio    = TItem (Radio0 Int)
 
instance HasInput TItem Radio0 Int where
 
  setValue (TItem (Radio0 _ _) n) i = tk_putTcl ["global", n, ";set ",n,show i]
 
  getValue (TItem (Radio0 _ _) n) =
    do { x <- tk_getTcl ["global", n, ";set ",n] ; tk_fromGUI x}
 
radio :: (Widget (c Radiobutton0))
      => [Conf Radio] ->[c Radiobutton0] -> GUI Radio
radio cs xs =
  do x <- tk_newPathName
     seqs (zipWith  (\m i -> csets m [const (Tk_Variable x),
                                      const (Tk_Value i)]) xs [0..])
     let rd = (TItem (Radio0 0 []) x)
     setValue rd 0                 -- initial setting
     csets rd cs 
     result rd

instance Widget Radio where
  cset w c = 
    case c w of
      Tk_InitValue a -> a
      _              -> done
  onArgs e args a w = Tk_FreeOption done


-- Menubutton ------------------------------------------------

data Menubutton0 = Menubutton0
type Menubutton  = WItem Menubutton0
type Cascade     = MItem Menubutton0
 
instance HasAnchor     Menubutton
instance HasHeight     Menubutton
instance HasWidth      Menubutton
instance HasFocus      Menubutton
instance HasBorder     Menubutton
instance HasPad        Menubutton
instance HasBackground Menubutton
instance HasForeground Menubutton
instance HasText       Menubutton 
instance HasCommand    Menubutton where
  command c _ = Tk_DummyConf
instance HasBackground Cascade
instance HasForeground Cascade
instance HasText       Cascade 
instance HasCommand    Cascade where
  command c _ = Tk_DummyConf
 
menubutton :: [Conf Menubutton] -> Window -> GUI Menubutton
menubutton cs t = tk_makeWItem defaults Menubutton0 t cs "menubutton"
  where defaults = tk_getWinDef t
 
cascade :: [Conf Cascade] -> Menu -> GUI Cascade
cascade cs t = tk_makeMItem defaults Menubutton0 t cs "cascade"
  where defaults = tk_getMenDef t


-- Checkbutton  ----------------------------------------------
 
data Checkbutton0 a = Checkbutton0 a
type Checkbutton  = WItem (Checkbutton0 Bool)
type MCheckbutton = MItem (Checkbutton0 Bool)
 
instance HasAnchor     Checkbutton
instance HasBorder     Checkbutton
instance HasFocus      Checkbutton
instance HasWidth      Checkbutton
instance HasHeight     Checkbutton
instance HasPad        Checkbutton
instance HasBackground Checkbutton
instance HasForeground Checkbutton
instance HasText       Checkbutton
instance HasCommand    Checkbutton
instance HasIndicator  Checkbutton
instance HasBackground MCheckbutton
instance HasForeground MCheckbutton
instance HasText       MCheckbutton
instance HasCommand    MCheckbutton
instance HasIndicator  MCheckbutton
instance GUIValue v => HasInput c Checkbutton0 v where
  getValue w =
    do s <- tk_getTcl ["global", tk_getVarName w, ";set",tk_getVarName w ]
       tk_fromGUI s
 
  setValue w s =
    tk_output (tk_toGUI s) $ \x ->
    tk_putTcl [ "global",tk_getVarName w,";"
              , "set",tk_getVarName w, x
              ]
 
checkbutton :: [Conf Checkbutton] -> Window -> GUI Checkbutton
checkbutton cs w =
  do c <- tk_makeWItem defaults (Checkbutton0 False) w [] "checkbutton"
     let cs' = [ \_ -> Tk_OffValue "False"
               , \_ -> Tk_OnValue "True"
               , \_ -> Tk_Variable (tk_getVarName c)
               ] in csets c (cs' ++ cs)
     result c
  where defaults = tk_getWinDef w
     
mcheckbutton :: [Conf MCheckbutton] -> Menu -> GUI MCheckbutton
mcheckbutton cs w =
  do c <- tk_makeMItem defaults (Checkbutton0 False) w [] "checkbutton"
     let cs' = [ \_ -> Tk_OffValue "False"
               , \_ -> Tk_OnValue "True"
               , \_ -> Tk_Variable (tk_getVarName c)
               ] in csets c (cs' ++ cs)
     result c
  where defaults = tk_getMenDef w
 

-- Separator -------------------------------------------------
 
data Separator0 = Separator0
type Separator  = MItem Separator0
 
 
separator :: Menu -> GUI Separator
separator t = tk_makeMItem [] Separator0 t [] "separator"
 

--------------------------------------------------------------------------
-- Canvas and Canvas Items

-- Canvas ----------------------------------------------

{- already defined in other module
data Canvas0 = Canvas0
type Canvas  = WItem Canvas0
-}
 
instance HasBackground Canvas
instance HasScroll     Canvas
instance HasFocus      Canvas
instance HasBorder     Canvas
instance HasWidth      Canvas
instance HasHeight     Canvas
 
canvas :: [Conf Canvas] -> Window -> GUI Canvas
canvas cs t = tk_makeWItem defaults Canvas0 t cs "canvas"
  where defaults = map tk_noForeground (tk_getWinDef t)
 
scrollRegion :: (Int,Int) -> Conf Canvas
scrollRegion (x,y) c = Tk_ScrollRegion (x,y)
 
clearCanvas :: Canvas -> GUI ()
clearCanvas w = do
  tk_toTcl w ["delete all"]
  tk_delBelowEvents (tk_getPathName w)

-- CArc --------------------------------------------------------

data CArc0 = CArc0
type CArc  = CItem CArc0

instance HasFillColor CArc

angleStart :: Int -> Conf CArc
angleStart i _ = Tk_AngleStart i

angleExtent :: Int -> Conf CArc
angleExtent i _ = Tk_AngleExtent i

carc :: (Int,Int) -> (Int,Int) -> [Conf CArc] -> Canvas -> GUI CArc
carc (x1,y1) (x2,y2) cs c = tk_makeCItem CArc0 c [x1,y1,x2,y2] cs "arc"


-- CPolygon ----------------------------------------------------

data CPolygon0 = CPolygon0
type CPolygon = CItem CPolygon0

instance HasFillColor CPolygon

cpolygon :: [(Int,Int)] -> [Conf CPolygon] -> Canvas -> GUI CPolygon
cpolygon xy cs c = tk_makeCItem CPolygon0 c xys cs "polygon"
   where xys = foldr (\(x,y) r -> x:y:r) [] xy

-- CPicture ----------------------------------------------------

data CPicture0 = CPicture0
type CPicture  = CItem CPicture0

cpicture :: (Int,Int) -> [Conf CPicture] -> Canvas -> GUI CPicture
cpicture (x,y) cs c = do
    tk_makeCItem CPicture0 c [x,y] cs "image"

image :: String -> Conf CPicture 
image n (CItem _ w i) = Tk_FreeOption $ 
  do x <- tk_getTcl ["image create photo"]
     tk_putTcl [x, "configure", "-file", n]
     tk_putTcl [w, "itemconfigure", show i," -image", x]

-- COval ------------------------------------------------------
 
data COval0 = COval0
type COval  = CItem COval0
 
instance HasFillColor COval
 
coval :: (Int,Int) -> (Int,Int) -> [Conf COval] -> Canvas -> GUI COval
coval (x1,y1) (x2,y2) cs c = tk_makeCItem COval0 c [x1,y1,x2,y2] cs "oval"


-- CLine ------------------------------------------------------
 
data CLine0 = CLine0
type CLine  = CItem CLine0
 
instance HasFillColor CLine where
  penColor  c _ = Tk_FillColor c
  fillColor _ _ = Tk_DummyConf
  
cline :: (Int,Int) -> (Int,Int) -> [Conf CLine] -> Canvas -> GUI CLine
cline (x1,y1) (x2,y2) cs c = tk_makeCItem CLine0 c [x1,y1,x2,y2] cs "line"
 

-- CRect -------------------------------------------------
 
data CRect0 = CRect0
type CRect  = CItem CRect0
 
 
crect :: (Int,Int) -> (Int,Int) -> [Conf CRect] -> Canvas -> GUI CRect
crect (x1,y1) (x2,y2) cs c =
  tk_makeCItem CRect0 c [x1,y1,x2,y2] cs "rectangle"
 
instance HasFillColor CRect


-- CText -----------------------------------------------------
 
data CText0 = CText0
type CText  = CItem CText0
 
instance HasText       CText where
  underline _ _ = Tk_DummyConf
  bitmap _ _    = Tk_DummyConf
instance HasBackground CText where
  background _ _ = Tk_DummyConf
instance HasForeground CText where
  foreground c _ = Tk_FillColor c
instance HasAnchor     CText
 
ctext :: (Int,Int) -> [Conf CText] -> Canvas -> GUI CText
ctext (x1,y1) cs c =
  tk_makeCItem CText0 c [x1,y1] (const (Tk_Anchor "w"):cs) "text"
 

-- CBitmap ----------------------------------------------------
 
data CBitmap0 = CBitmap0
type CBitmap  = CItem CBitmap0
 
instance HasBackground CBitmap 
instance HasForeground CBitmap where
  font _ _ = Tk_DummyConf
instance HasAnchor     CBitmap
instance HasText       CBitmap where
  text      _ _ = Tk_DummyConf
  underline _ _ = Tk_DummyConf


cbitmap :: (Int,Int) -> [Conf CBitmap] -> Canvas -> GUI CBitmap
cbitmap (x1,y1) cs c = tk_makeCItem CBitmap0 c [x1,y1] cs "bitmap"


--------------------------------------------------------------------------
-- Special Widgets

-- Clipboard --------------------------------------------------------

data Clipboard0 a = Clipboard0 (GVar a)
type Clipboard a  = TItem (Clipboard0 a)

instance Widget (Clipboard a) where
  cset w c = void $ tk_showConf "dummy" (c w)
  cget w f = result tk_defaultValue
  onArgs _ _ _ _ = Tk_DummyConf

instance HasInput TItem Clipboard0 a where   
  getValue (TItem (Clipboard0 v) _)   = readGVar v   
  setValue (TItem (Clipboard0 v) _) x = writeGVar v x
  readOnly _ _ = Tk_DummyConf

clipboard :: Widget (Clipboard a) => [Conf (Clipboard a)] -> GUI (Clipboard a)
clipboard cs = 
  do v <- newGVar (error "Clipboard not initialized")
     let t = TItem (Clipboard0 v) "clipboard"
     csets t cs
     result t

----------------------------------------------------------------------
-- Composing Widgets
 
composeWidget :: (Widget (WItem v), Widget (WItem w))
              => w -> WItem v -> [Conf (WItem w)] -> GUI (WItem w)
composeWidget w v cs =
  let (WItem _ node) = tk_WidgetToFrame v
      a = WItem w node
  in do {csets a cs ; result a}
 
getWidget :: WItem w -> w
getWidget (WItem a _) = a

-- input mask ------------------------------------------
-- combination of a label and a entryfield:


data Input0 v = Input (Entry v,Label)
type Input  v = WItem (Input0 v)

input :: (Widget (Input v), GUIValue v)
      => [Conf (Input v)] -> Window -> GUI (Input v)
input cs w =
  do l <- label [] w
     e <- entry [] w
     composeWidget (Input (e,l)) (l <-< expand e) cs

instance Widget (Entry v) => Widget (Input v) where
  cset w c = let newc = c w
             in case newc of
                  (Tk_Text s)      -> cset (inputL w) (const (Tk_Text s))
                  (Tk_InitValue s) -> cset (inputE w) (const (Tk_InitValue s))
                  _                -> do cset (inputE w) (const newc)
                                         cset (inputL w) (const newc)

  cget w f = let cs = f tk_defaultValue w
             in cget (inputL w) (\_ _ -> cs)

  onArgs e r a w = onArgs e r a (inputE w)

instance HasFocus (Entry v) => HasFocus (Input v) where
  focus = focus . inputE

instance HasWidth      (Input v)
instance HasBackground (Input v)
instance HasForeground (Input v)
instance HasText       (Input v)
instance HasBorder     (Input v)

instance (HasInput WItem Entry0 v) => HasInput WItem Input0 v where
  getValue = getValue . inputE
  setValue = setValue . inputE

inputE :: Input v -> Entry v
inputE i = let (Input (e,l)) = getWidget i in e

inputL :: Input v -> Label
inputL i = let (Input (e,l)) = getWidget i in l

--------------------------------------------------------------------------

-- embedded windows : poor version ---------------------------------------

data EWindow0 = EWindow0
type EWindow  = TItem EWindow0

class HasEmbeddedWindow c where  --  ### canvas and positions
  ewindow :: (Int,Int) -> [Conf Frame] -> c -> GUI Window

instance HasEmbeddedWindow Edit where
  ewindow p cs t =
    do w <- tk_newPathName
       let m = tk_getPathName t
       tk_putTcl ["pack [","frame", m++w, "]"]
       tk_toTcl t ["window create", tk_toGUI p, "-window", m++w]
       csets (WItem Frame0 (Tk_Node (m,w,[],[]) [tk_emptyNode])) cs
       result (TItem (Window0 []) (m++w))

instance HasEmbeddedWindow Canvas where
  ewindow (x,y) cs t =
    do w <- tk_newPathName
       let m = tk_getPathName t
       tk_putTcl ["pack [","frame", m++w, "]"]
       tk_toTcl t ["create window" , show x, show y
                  , "-window", m++w
                  ]
       csets (WItem Frame0 (Tk_Node (m,w,[],[]) [tk_emptyNode])) cs
       result (TItem (Window0 []) (m++w))

--------------------------------------------------------------------------
-- Useful functions

-- updating the screen

updateIdleTask :: GUI ()
updateIdleTask = tk_putTcl ["update idletasks"]

updateTask :: GUI ()
updateTask = tk_putTcl ["update"]

-- adding new configuration options -------------------------------------


myConf :: Widget w => (w -> GUI a) -> Conf w
myConf f w = Tk_FreeOption (void (f w))

-- Destroying Widgets  ---------------------------------

destroy :: Widget w => w -> GUI () -- throw widget and all the events away
destroy w = do
  let x = tk_getPathName w
  tk_delEvents x
  tk_putTcl ["destroy", x]

-- Implementation Class --------------------------------

class TkWidget w where
  tk_getPathName ::  w -> String   

  tk_getVarName :: w -> String    
  tk_getVarName = tk_getPathName

--------------------------------------------------------------------------
-- Implementations Specific Code


--------------------------------------------------------------------------
-- Some primitives

tk_fromTcl :: TkWidget w => w -> [String] -> GUI String
tk_fromTcl w xs = tk_getTcl (tk_getPathName w : xs)
 
tk_toTcl :: TkWidget w => w -> [String] -> GUI ()
tk_toTcl w xs = tk_putTcl (tk_getPathName w : xs)
 
tk_output :: String -> (String -> GUI a) -> GUI a
tk_output s f = tk_melt $
  do liftIO $ primSetVar s
     f ("$tmp")
     
--------------------------------------------------------------------------
-- the end.
