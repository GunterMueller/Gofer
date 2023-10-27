-- filebox dialog ---------------------------------
--
-- simple browser for directories
--
-- Ton Vullinghs   March 1996

 
-- scrolling widgets -----------------------------------
 
data ScrollRB w = ScrollRB (WItem w)
 
instance Widget (WItem w) => Widget (WItem (ScrollRB w)) where
  cset w c = cset (scrollW w) (const (c w))
 
scrollW (WItem (ScrollRB v) _) = v

scrollRB t cs w =
  do { x <- w
     ; r <- vscroll (cs ++ [relief "raised"]) x
     ; b <- hscroll (cs ++ [relief "raised"]) x
     ; composeWidget (ScrollRB x) (flexible (flexible x <|< r) ^-^ b) []
     }   
 
-- filebox ---------------------------------------------
 

filebox :: (String -> GUI ())    -- succeed action
        -> (String -> GUI ())    -- cancel action
        -> String                -- initial path
        -> String                -- initial selection pattern
        -> [Conf Frame]          -- default configuration
        -> GUI ()

filebox ok cancel path mask confs = 
  do { fb_win <- window [title "file select"] 
     ; fb_box <- scrollRB fb_win [] (listbox [] fb_win)
     ; fb_bar <- control fb_win (scrollW fb_box)
     ; pack (flexible (flexible fb_box ^-^ fb_bar)) 
     }

  where

     control w l =
        do { home <- pwd                              -- save actual path
           ; i1   <- input [ text "Pathname:"
                           , initValue path
                           ] w
           ; i2   <- input [ text "Selection pattern:"
                           , initValue mask
                           ] w
           ; cset i1 $ on return (scan i1 i2 l)
           ; cset i2 $ on return (scan i1 i2 l)
           ; cset l (on (doubleClick 1) (nextOrOk w l i1 i2 ok home))
           ; b1 <- button [ text "Ok"                 -- ok button
                          , command (okOrCancel w l ok home)
                          ] w
           ; b2 <- button [ text "Rescan"             -- rescan button
                          , command (scan i1 i2 l)
                          ] w
           ; b3 <- button [ text "Cancel"             -- cancel button 
                          , command (okOrCancel w l cancel home)
                          ] w
           ; scan i1 i2 l
           ; frame confs ( (i1 ^*-^ i2)                       -- layout
                         ^*-^ 
                         (b1 <*-< b2 <*-< b3)
                   ) 
           }

     okOrCancel w l f home = 
       do { s <- getSelectedFile l 
          ; cdDir home 
          ; closeWindow w
          ; f s
          }

     scan i1 i2 l = 
       do { path <- getValue i1
          ; b <- fileIsDir path
          ; if b
            then do { mask <- getValue i2
                    ; ls <- getDir mask
                    ; setValue l ls 
                    }
            else do { p <- pwd
                    ; setValue i1 p
                    ; scan i1 i2 l
                    }
          }

     getSelectedFile l =
       do {ys <- getSelection l
          ;case ys of
                []     -> result ""                -- no file selected
                (x:xs) -> do { [a] <- getFromTo l x x
                             ; p  <- pwd
                             ; result (p ++ "/" ++ a)
                             }
          }


     nextOrOk w l i1 i2 ok home =
       do {s <- getSelectedFile l
          ;b <- fileIsDir s
          ;if b then do { cdDir s
                        ; p <- pwd
                        ; setValue i1 p
                        ; scan i1 i2 l
                        } 
                else okOrCancel w l ok home 
          }

