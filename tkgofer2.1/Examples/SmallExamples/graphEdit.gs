--------------------------------------------------------------------------
--                   GraphEdit 12/95 by T. Schwinn
--          (based on TK-TCL 'graph' from J. K. Ousterhout) 
--------------------------------------------------------------------------
-- 
-- start with `main`
--
-- left mouse button  : create node
-- middle mouse button: move node
-- `1`                : select first node for connection
-- `2`                : select second node for connection
--
--------------------------------------------------------------------------




-- type definitions , acronyms -------------------------------------------

type Pos    = (Int,Int)                      
type Node   = (COval,Pos)
type Edge   = (CLine,Pos,Pos)            
type Status = Clipboard ([Edge],[Node],[Node]) -- Edges, Node "1" or no Node,highlighted node or n.

instance Num Pos where
  (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)

black =  "black"
white =  "white"

leave = Tk_Event "<Any-Leave>"
enter = Tk_Event "<Any-Enter>"

-- main window & program -------------------------------------------------

main :: IO ()
main = start $ 
  do { w  <- window [title "graph editor"]
     ; st <- clipboard [ initValue ([],[],[]) ]
     ; c  <- canvas [background "green"] w
     ; csets c [ onArgs (click 1) "xy" (node c st)
               , on (key "1") (startEdge st)
               , on (key "2") (endEdge c st)
               , onArgs (motion 2) "xy" (moveIt st)
               ]
     ; focus c
     ; pack c
     }

-- put nodes & edges -----------------------------------------------------

node :: Canvas -> Status -> [String] -> GUI ()
node can st [x',y'] = 
  do { (x,y) <- result (numval x',numval y')
     ; o <- coval (x-10,y-10) (x+10,y+10) [ penColor black 
                                          , fillColor white
                                          ] can
     ; csets o [ on enter (enterNode st o) 
               , on leave (leaveNode st)
               ] 
     }

startEdge :: Status -> GUI ()
startEdge st = 
  do { (es,o,h) <- getValue st
     ; if (length o) > 0 then cset ((fst . head) o) (fillColor "white") else done
     ; if (length h) > 0 then cset ((fst . head) h) (fillColor "red") else done
     ; setValue st (es,h,h)
     }

endEdge :: Canvas -> Status -> GUI ()
endEdge c st = 
  do { (es,f,h) <- getValue st
     ; if (((length f) /= 0) && ((length h) /= 0))
       then do { h' <- result  ((snd.head) h)
               ; f'' <- result ((fst.head) f)
               ; f''' <- getCoords f''
               ; f'  <- result ((head f''') + (10,10))
               ; l <- cline h' f' [] c
               ; lowerObject l
               ; setValue st ((l,h',f'):es,[(f'',f')],h)
               }
        else done
     }

-- highlight node --------------------------------------------------------

enterNode :: Status -> COval -> GUI ()
enterNode st g =  
  do { (es,f,o) <- getValue st
     ; if (length o) /= 0 then do { o' <- result ((fst.head) o)
                                  ;  c <- cget o' fillColor
                                  ; if c == "red" then done else cset o' (fillColor white)
                                  } else done
     ; h <- getCoords g
     ; setValue st (es,f,[(g,(head h) + (10,10))])
     ;  c <- cget g fillColor
     ; if c == "red" then done else cset g (fillColor black)
     }

leaveNode :: Status -> GUI ()
leaveNode st = 
  do { (es,f,o) <- getValue st
     ; if (length o) == 0 then done 
       else do { o' <- result ((fst.head) o)
               ;  c <- cget o' fillColor
               ; if c == "black" then cset o' (fillColor white) else done
               ; setValue st (es,f,[])
               }
     }

-- move node and adjacent edges ------------------------------------------

moveIt :: Status -> [String] -> GUI ()
moveIt st [x',y'] = 
  do { (x,y) <- result ((numval x', numval y'))
     ; (es,fs,hs) <- getValue st
     ; if (length hs) == 0 then done
       else do { (h',h) <- result (head hs)
               ; moveObject h' ((x,y)-h)
               ; let ess = [ (e,t) | (e,f,t) <- es, f == h ] ++ 
                           [ (e',f') | (e',f',t') <- es, t' == h ]
                 in seqs (map (\(e,t) -> setCoords e [(x,y),t]) ess)
               ; let es' = [ (e,c f,c t) | (e,f,t) <- es ] 
                     c z = if z == h then (x,y) else z
                 in setValue st (es',fs,[(h',(x,y))])
               ; updateTask
               }
     }


