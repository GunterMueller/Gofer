---------------------------------------------------------
-- Example     : book.gs
-- Application of input masks/definition of new widgets
-- Start with  : main
---------------------------------------------------------

type Book = 
  ( String      -- author
  , String      -- title
  , Int         -- year of publication
  , Bool        -- reservation 
  )

data BookMask0 v = 
  BookMask ( Input String 
           , Input String 
           , Input Int
           , Checkbutton
           ) v

type BookMask = WItem (BookMask0 Book)

-- create a new mask --------------------------------

bookmask :: Window -> [Conf BookMask] -> GUI BookMask
bookmask w cs =
  do {n1 <- input [text "author :"] w
     ;n2 <- input [text "title  :"] w
     ;n3 <- input [text "year   :"] w
     ;bb <- checkbutton [text "make reservation"] w
     ;cset n1 (on return $ do {x <- getValue n1; focus n2})  -- get checks value
     ;cset n2 (on return $ do {x <- getValue n2; focus n3})
     ;cset n3 (on return $ do {x <- getValue n3; focus n1})
     ;l <- label [text "Registration Form"] w
     ;f <- frame [ background "red"
                 , borderWidth 4
                 , relief "raised"
                 ] (l ^-^ n1 ^-^ n2 ^-^ (n3 << bb))
     ;composeWidget (BookMask (n1,n2,n3,bb) ("","",1900,False)) f cs
     }

-- a mask is a widget -------------------------------

instance Widget BookMask where
   cset w@(WItem (BookMask (n1,n2,n3,bb) _) _)  c =
     let cs = c w in
     do { cset n1 (const cs)
        ; cset n2 (const cs)
        ; cset n3 (const cs)
        ; cset bb (const cs)
        }

-- specify reading and writing for masks -------------

instance HasInput WItem BookMask0 Book where
  getValue (WItem (BookMask (n1,n2,n3,bb) _) _) =
     do {x <- getValue n1
        ;y <- getValue n2
        ;z <- getValue n3
        ;b <- getValue bb
        ;result (x,y,z,b)
        }

  setValue (WItem (BookMask (n1,n2,n3,bb) _) _ ) (a,b,c,d) =
     do { setValue n1 a
        ; setValue n2 b
        ; setValue n3 c
        ; setValue bb d
        }

instance Text Book

-- specify conversion routines -------------------------

instance GUIValue Book where
  tk_defaultValue    = ("","",1900,False)
  tk_toGUI (a,b,c,d) = 
    tk_toGUI (a ++ "::" ++ b ++ "::" ++ show c ++ "::" ++ show d )
  tk_convert xs    = 
    let (a, r1) = breakAt xs
        (t, r2) = breakAt r1
        (y, b ) = breakAt r2
    in Tk_Ok (a,t,convert' y,convert' b)
    where breakAt t = let (a,b) = break (==':') t
                      in (a,dropWhile (== ':') b)
          convert' y = let x = tk_convert y in
                       case x of 
                         Tk_Err a -> error "error in book conversion"
                         Tk_Ok  a -> a

-- the actual application -------------------------------

applicationDefaults :: [Conf Default]
applicationDefaults = [font "12x24", background "LightYellow"]

main =
 start $ library 

library =
  do { lib <- windowDefault [title "Library"] applicationDefaults
     ; newbook       <- bookmask lib []
     ; (books, box)  <- booklist lib
     ; buttons       <- control lib newbook box
     ; menus         <- menubar lib newbook box
     ; cset box (on (doubleClick 1) (showbook newbook box))
     ; pack (flexible (menus ^-^ newbook ^-^ buttons ^-^ flexible books))
     }

showbook newbook box =
  do {y:ys <- getSelection box
     ;sels <- getFromTo box y y
     ;setValue newbook (head sels)
     }

replacebook newbook box =
  do {ys <- getSelection box
     ;case ys of
        (x:xs) -> do {delFromTo box x x
                     ;b <- getValue newbook
                     ;putPos box x [b]
                     }
        []     -> done
     }

menubar lib newbook box =
  do {mb1 <- menubutton [text "File", relief "raised"] lib
     ;m1  <- menuDefault [] applicationDefaults mb1
     ;b11 <- mbutton [text "New", command (setValue box [])] m1
     ;b12 <- mbutton [text "Reset", command (setValue box database)] m1
     ;b13 <- mbutton [text "Quit", command quit] m1
     ;mb2 <- menubutton [text "Search", relief "raised"] lib
     ;m2  <- menuDefault [] applicationDefaults mb2
     ;b21 <- mbutton [ text "Reserved" 
                        , command $ showall box True
                        ] m2
     ;b22 <- mbutton [text "Not Reserved"
                        , command $ showall box False
                        ] m2
     ;l   <- label [relief "raised"] lib
     ;result (mb1 << mb2 << flexible l)
     }

showall box p  =
 do {w <- windowDefault [title "show"] applicationDefaults
    ;as <- getValue box
    ;l <- listbox [initValue [(a,b,c,d) | (a,b,c,d) <- as, d == p]] w
    ;s1 <- vscroll [] l
    ;s2 <- hscroll [] l
    ;l' <- frame [] (s1 <|< l ^-^ s2)
    ;b <- button [text "OK", command (closeWindow w)] w
    ;pack (flexible (flexible l' ^-^ b))
    }

control lib newbook box =
  do { b1 <- button [ text "Clear"
                        , command (setValue newbook ("", "",1900,False))
                        ] lib
     ; b2 <- button [ text "Add"
                        , command $ do { y <- getValue newbook
                                       ; putEnd box [y] 
                                       }
                        ] lib
     ; b3 <- button [ text "Replace"
                        , command (replacebook newbook box)
                        ] lib
     ; f <- frame [ background "red", relief "raised", borderWidth 4]
                  (b1 <*-< b2 <*-< b3) 
     ; result f
     }

booklist w =
  do { l  <- label   [text "Database"] w
     ; lb <- listbox [width 30, initValue database] w
     ; s1 <- vscroll [] lb 
     ; s2 <- hscroll [] lb
     ; f  <- frame 
                 [ background "red"
                 , relief "raised"
                 , borderWidth 4
                 ] (l ^-^ ex (ex (ex lb <|< s1) ^-^ s2)) 
     ; result (f , lb)
     }
  where ex a = flexible a

database :: [Book]
database =
  [("Aho", "Compilers", 1986, False),
   ("Jones","Gofer",1992,False),
   ("Knuth","The TeX Book", 1984, False),
   ("Knuth","Literate Programming", 1984, False),
   ("Ousterhout","Tcl and Tk", 1994, False)
  ]

