-- embedded windows: demo

main :: IO ()
main = start $ 
  do w <- window [title "Embedded windows"]
     demo w

demo :: Window -> GUI ()
demo w = 
  do c <- canvas [background "white", width 400, height 400] w
     pack c
     cset c (onxy (click 3) (newWindow c))
     putCircles c

putCircles :: Canvas -> GUI ()
putCircles c = 
  do seqs (copy 20 (do
     x1 <- random 200 
     y1 <- random 200 
     w  <- random 20
     cl <- random 5
     let color = ["blue","red","black","yellow","green"]!!cl
     void (coval (x1,y1) (x1+w+20,y1+w+20) [fillColor color] c)))

newWindow :: Canvas -> (Int,Int) -> GUI ()
newWindow c (x,y) = do
  w <- ewindow (x,y) [] c
  e <- edit  
        [ initValue "Take care, this is an editor!\nDo You Like Recursion?"
        , height 7 , width 40
        ] w 
  l <- vscroll [] e
  b <- button [ text "close window"
              , command (closeWindow w)
              ] w
  pack ((l <|< e) ^-^ b)

  p <- getMark e insMark
  v  <- ewindow p [] e
  b' <- button [ text "Yes", command (demo v)] v
  putEndTag e "I like it!" [foreground "red"] 
  pack b'
  
  


