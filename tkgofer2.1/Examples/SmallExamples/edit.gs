

type EState = Clipboard (String,Int)  -- buffer, fontsize

ex_edit :: IO ()
ex_edit = start $ do 
  st <- clipboard [ initValue ("",18) ]
  w  <- window [title "Write !!"]
  e  <- edit [width 40, height 15, wrap True,
              background "white", font "times-roman18"] w
  s  <- vscroll []  e
  f  <- frame [borderWidth 4] (flexible e <|< s) 
  bs <- menubar  
          [("File", fileM e), ("Edit", editM e st), ("Style", styleM e st)] w
  pack (flexible (horizontal bs ^-^ flexible f))


menubar :: [(String, Menu -> [GUI ()])] -> Window -> GUI [Menubutton]
menubar xs w =
 let (ss,fs) = unzip xs 
 in do bs <- binds [menubutton [text s] w | s <- ss]
       ms <- binds [menu [] b | b <- bs]
       (seqs . concat) [f m | (f,m) <- zip fs ms] 
       result bs

cmd :: (String, GUI ()) -> Menu -> GUI ()
cmd (s,c) m = void (mbutton [text s, command c] m)

fileM :: Edit -> Menu -> [GUI ()]
fileM e m =
  [cmd s m | s <- [("New", doNew), ("Quit", doQuit)]]
  where doNew  = warning "Really Clear?" (setValue e "") 
        doQuit = warning "Really Quit?" quit 


warning :: String -> GUI () -> GUI ()
warning msg yes = do
   w  <- windowDefault [title "Warning"][font "helvetica18"]
   m  <- message [text msg, relief "ridge"] w
   b1 <- button [text " Yes ", command (closeWindow w `seq` yes)] w
   b2 <- button [text " No " , command (closeWindow w)] w
   f  <- frame [borderWidth 2, relief "ridge"] (b1 << b2)
   focus b2
   pack (m ^*+^ f)


editM :: Edit -> EState -> Menu -> [GUI ()]
editM e st m =
  cset e (onXY (click 3) (\xy -> popup xy m)) :
  [cmd s m | s <- [("cut", doCut), ("copy", doCopy), ("paste", doPaste)]]
  where doCut = selectionExists e ==> do
          ([p,q],t) <- getMarkedPart e
          delFromTo e p q
          updValue (\(_,i) -> (t,i)) st 
        doCopy = do
          (_,t) <- getMarkedPart e
          updValue (\(_,i) -> (t,i)) st
        doPaste = do
          (t,_) <- getValue st
          p <- getMark e insMark
          putPos e p t

selectionExists :: Edit -> GUI Bool
selectionExists e = do 
  ps <- getSelection e
  result (ps /= [])


styleM :: Edit -> EState -> Menu -> [GUI ()]
styleM e st m = fonts ++ [void (separator m), subm]
  where fonts =
          [cmd (s ,setf ("times-"++s)) m | s <- ["roman", "bold", "italic"]]
        subm = do
          mb <- cascade [text "font size"] m
          m  <- menu [] mb
          bs <- binds [mradiobutton
                  [text (show n), command (updValue (\(t,_) -> (t,n)) st)] m
                      | n <- [8,10..24]]
          void (radio [] bs)
        setf s = selectionExists e ==> do
          (ps,_) <- getMarkedPart e
          (_,n)  <- getValue st
          void (tag ps [font (s++show n)] e)
  
getMarkedPart :: Edit -> GUI ([(Int,Int)],String)
getMarkedPart e = do 
  ps <- getSelection e
  case ps of [a,b]     -> do tx <- getFromTo e a b
                             result (ps,tx)
             otherwise -> result ([],"")

