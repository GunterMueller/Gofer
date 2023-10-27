-------------------------------- L'le Paint ------------------------------------
--                            = Little Paint                                  --
--                           1/96  by T.Schwinn                               --
--------------------------------------------------------------------------------

--------------------------- type declaration -----------------------------------

data WorkMode = DrawM
              | CircleM
              | LineM
              | RectangleM
              | RubberM

type PaintState = Clipboard (String,Int,Int,Int,WorkMode,Bool)
  --     : current color
  -- Int      : X-position of last point
  -- Int      : Y-position of last point
  -- Int      : current line width
  -- Workmode : active tool
  -- Bool     : True = color selection is open  #### weg

------------------------------ main program ------------------------------------

main :: IO ()
main = start $ malProg

malProg :: GUI ()
malProg = 
  do { s <- clipboard [ initValue ( "magenta",100,100,1,DrawM,False) ]
     ; w <- window [title "L'le Paint"] 
     ; board <- canvas [background ( "White")] w
     ; csets board [ onArgs (motion 1) "xy" (doLeftMotion s board)
                   , onArgs (click 1) "xy" (doLeft s board)
                   , onArgs (click 3) "xy" (doRight s board)
                   , onArgs (click 2) "xy" (doMiddle s board)
                   ]
     ; (stat,colstat,statbox) <- statusline s w board
     ; cmdbox <- iconbar s w stat colstat
     ; pack (board <|< cmdbox ^-^ statbox)
     }

-------------------------------- main window -----------------------------------

bmp s = bitmap ("Bitmaps/"++s)

--bitmap s w = Bitmap s
--aspect i w = Aspect i

iconbar :: PaintState -> Window -> Label -> Label -> GUI Frame
iconbar s w l lc = 
  do { bs <- binds [button [bmp n,command c] w | (n,c) <- ib ]
     ; result (matrix 1 bs)
     } where ib = [ ("pensmall2.bit",setWorkMode s l DrawM)
                  , ("grid.bit",setWorkMode s l LineM)
                  , ("circle.bit",setWorkMode s l CircleM)
                  , ("rectangle.bit",setWorkMode s l RectangleM)
                  , ("palette.bit",colors s lc)
                  , ("width.bit",selectWidth s)
                  , ("rubber.bit",setWorkMode s l RubberM)
                  , ("printer.bit",done)
                  ]
                     
statusline :: PaintState -> Window -> Canvas -> GUI (Label,Label,Frame)
statusline s w cv = 
  do { l1 <- label [bmp "pensmall2.bit"] w
     ; l2 <- label [] w
     ; l3 <- label [text "     ",background ( "magenta")] w
     ; l4 <- label [] w
     ; l5 <- button [text "Clear",command (clearCanvas cv)] w
     ; l6 <- label [] w
     ; b1 <- button [text "Info",command showInfo] w
     ; l7 <- label [] w
     ; b2 <- button [text "Help",command showHelp] w
     ; l8 <- label [] w
     ; b3 <- button [text "Exit",command quit] w
     ; f <- frame [] (l1<<l2<<l3<<l4<<l5<<l6<<b1<<l7<<b2<<l8<<b3)
     ; result (l1,l3,f)
     }

------------------------------ info page -------------------------------------
          
showInfo :: GUI ()
showInfo = 
  do { w <- window [title "Info"] 
     ; m <- message [text infoText,justify "center",aspect 200] w
     ; b <- button [text "Ok",command (closeWindow w)] w
     ; pack (m^-^b)
     }  
infoText = 
 "L'le Paint \n \n 1/96 by T. Schwinn \n Abteilung \n"++
 "Programmiermethodik \n Universitaet Ulm"

-------------------------------- help page -------------------------------------

showHelp :: GUI ()
showHelp = 
  do { w <- window [title "Help"] 
     ; m <- message [text helpText,aspect 100] w
     ; b <- button [text "Ok",command (closeWindow w)] w
     ; pack (m^-^b)
     } 

helpText=
 "Brief Help\n==========\n\n"++
 "Paint:\nMove mouse and press left button.\n\n"++
 "Line:\nSet startpoint with left button and endpoint with right button."++
 "\n\n"++
 "Circle:\nSet left upper vertex of a surrounding box with left button"++
 " and right lower vertex with middle button for a filled oval or "++
 "right button for an oval. :)\n\n"++
 "Rectangle:\nSet left upper vertex by pressing the left button. Set the "++
 "right lower vertex either by pressing the right button for a rectangle or"++
 " by pressing the middle button for a filled rectangle.\n\n"++
 ":\nSelect color in color dialogue.\n\n"++
 "Line width:\nEnter line width.\n\n"++
 "Rubber:\nPress left button to delete item.\n\n"++
 "Print\nNot supported yet." 

--------------------- color selection & dialogue -------------------------------

colors :: PaintState -> Label -> GUI ()
colors p l1 = 
  do { w <- window [title "Select color"] 
     ; l <- listbox [] w
     ; anz <- label [borderWidth 10] w
     ; cset l (on (doubleClick 1) (
               do {s <- tk_getTcl ["selection get"];setCol p [anz,l1] s}))
     ; setValue l ["black","blue","brown","green","magenta","orange","pink","white","yellow"]
     ; sr <- vscale [scaleRange (0,255)] w
     ; sg <- vscale [scaleRange (0,255)] w
     ; sb <- vscale [scaleRange (0,255)] w
     ; cset sr (command (do { s <- getCol sr sg sb; setCol p [anz,l1] s}))
     ; cset sg (command (do { s <- getCol sr sg sb; setCol p [anz,l1] s}))
     ; cset sb (command (do { s <- getCol sr sg sb; setCol p [anz,l1] s}))
     ; pack (((sr<<sg<<sb)^-^anz)<|<l)
     } 

{- color selection & dialogue actions -}

getCol r g b = 
  do { sr <- getValue r 
     ; sg <- getValue g
     ; sb <- getValue b
     ; result (rgb sr sg sb)
     } 

setCol :: PaintState -> [Label] -> String  -> GUI ()
setCol p ls col = 
  do { seqs [cset l (background col)| l <- ls]
     ; (_,n1,n2,n3,n4,n5) <- getValue p
     ; setValue p (col,n1,n2,n3,n4,n5)
     } 

----------------------------- line width dialogue ------------------------------

selectWidth :: PaintState -> GUI ()
selectWidth p = 
  do { (_,_,_,wi,_,_) <- getValue p
     ; w <- window [title "Select line width"]
     ; e <- entry [ width 10,relief "ridge"] w
     ; l <- label [text ("Width:"++show wi),width 10] w
     ; cset e (on return (
               do { (p1,p2,p3,wi,p4,p5) <- getValue p
                  ; s <- getValue e
                  ; s' <- result (let n = numval s in if n == 0 then 1 else n)
                  ; cset l (text (show s'))
                  ; setValue p (p1,p2,p3,s',p4,p5)
                  }      )
              )
     ; pack (l<<e)
     }

--------------------------- simple GUI ()s -------------------------------------

{- main program actions -}

doLeft :: PaintState -> Canvas -> [String] -> GUI ()
doLeft p cv [x',y'] = 
  do { (c,xo,yo,w,wm,gm) <- getValue p
     ; (x,y) <- result (numval x',numval y')
     ; case wm of
         DrawM      -> setPointAtMouse p x y
         CircleM    -> setPointAtMouse p x y
         LineM      -> setPointAtMouse p x y
         RectangleM -> setPointAtMouse p x y
         RubberM    -> tk_toTcl cv ["delete [ ",tk_getPathName cv,"find closest",x',y',"]"]
         _          -> done 
     }       
               
doMiddle :: PaintState -> Canvas -> [String] -> GUI ()
doMiddle p cv [x',y'] = 
  do { (c,xo,yo,w,wm,gm) <- getValue p
     ; (x,y) <- result (numval x',numval y')
     ; case wm of
         CircleM    -> (noR (coval (xo,yo) (x,y) [fillColor c,penColor c] cv))
         RectangleM -> (noR (crect (xo,yo) (x,y) [fillColor c,penColor c] cv))
         _          -> done 
     } 

doRight :: PaintState -> Canvas -> [String] -> GUI ()
doRight p cv [x',y'] = 
  do { (c,xo,yo,w,wm,gm) <- getValue p
     ; (x,y) <- result (numval x',numval y')
     ; case wm of
         CircleM    -> (noR (coval (xo,yo) (x,y) [penWidth w,penColor c] cv))
         LineM      -> drawLine p cv x y
         RectangleM -> (noR (crect (xo,yo) (x,y) [penWidth w,penColor c] cv))
         _          -> done 
     } 

noR x = do { _ <- x ; done }

doLeftMotion :: PaintState -> Canvas -> [String] -> GUI ()
doLeftMotion p cv [x,y] = 
  do { (c,xo,yo,w,wm,gm) <- getValue p
     ; case wm of
         DrawM -> drawLine p cv (numval x) (numval y)
         _     -> done 
     } 

drawLine :: PaintState -> Canvas -> Int -> Int -> GUI ()
drawLine p cv x y = 
  do { (c,xo,yo,w,wm,gm) <- getValue p
     ; _ <- cline (xo,yo) (x,y) [penWidth w,penColor c] cv
     ; setValue p (c,x,y,w,wm,gm)
     }

setWorkMode :: PaintState -> Label -> WorkMode -> GUI ()
setWorkMode p l m = 
  do { (c,xo,yo,w,wm,gm) <- getValue p 
     ; setValue p (c,xo,yo,w,m,gm)
     ; case m of
         DrawM      -> cset l (bmp "pensmall2.bit")
         CircleM    -> cset l (bmp "circle.bit")
         RectangleM -> cset l (bmp "rectangle.bit")
         LineM      -> cset l (bmp "grid.bit")
         RubberM    -> cset l (bmp "rubber.bit")
         _          -> done 
     }

setPointAtMouse :: PaintState -> Int -> Int -> GUI ()
setPointAtMouse p x y = 
  do { (c,n1,n2,n3,n4,n5) <- getValue p
     ; setValue p (c,x,y,n3,n4,n5)
     }


-------------------------------------- end -------------------------------------
