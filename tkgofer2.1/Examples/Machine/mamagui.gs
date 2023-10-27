----------------------------------------------------------------------------
-- MaMa GUI
-- 12/95,1/96 by Thilo Schwinn
-- Abteilung Programmiermethodik
-- Universitaet Ulm
----------------------------------------------------------------------------

type Program = [(String,Instruction)]
type Status  = Var (Machine,Program)

segment w s n = 
  do l <- label [text s, relief "ridge", labcol] w
     b <- listbox [background (id "white"), width n] w
     v <- vscroll [] b
     f <- frame [borderWidth 3, background (id "blue"), relief "raised"] 
            (l ^-^ flexible (flexible b <|< v))
     result (flexible f,b)

registers w = 
  do l <- label [text "REGISTERS", relief "ridge", labcol] w
     r <- binds [label [width 5,text s, relief "ridge"] w |
               s <- ["PC:","","SP:","","FP:","","GP:","","State:","","Count","0"]]   

     f <- frame [borderWidth 3, background "blue", relief "raised"]  
            (l ^-^ matrix 2 r)
     result (f,r)

controls w s prog heap stack regs lamaedit = 
  do l <- label [text "CONTROL", relief "ridge", labcol] w
     b_step <- button [text "Step"
                      ,command (do {x <- stepit s; showState s regs prog heap stack})] w 
     b_new <- button [text "Load",command (doload lamaedit)] w
     b_run <- button [text "Run",command (runit s regs prog heap stack)] w
     b_res <- button [text "Reset",command (dores s regs prog heap stack)] w
     b_com <- button [text "Compile",command (docomp s lamaedit regs prog heap stack)] w
     b_qut <- button [text "Quit",command quit] w
     frame [background (id "blue"), borderWidth 3, relief "raised"]
           (l ^-^ vertical (map fillX [b_new,b_com,b_step,b_run,b_res,b_qut]))
               
    

main :: IO ()
main = start $
  do s <- newGVar (machineReset,[])
     w <- windowDefault [title "MaMa"][font "courier-bold18"]
     ll <- label [text "LAMA INPUT", relief "ridge" , background "yellow"] w
     lt <- edit [height 8, background "pink"] w
     sb <- vscroll [] lt
     (rbox, regs)  <- registers w
     (pbox, prog)  <- segment w "MAMA PROGRAM" 10
     (hbox, heap)  <- segment w "HEAP"         30
     (sbox, stack) <- segment w "STACK"       10
     bbar <- controls w s prog heap stack regs lt
     pack (flexible 
            ((rbox  ^-^ bbar) <|< flexible
            (pbox <|< flexible (hbox <*+< sbox))) ^-^ (ll ^-^ (lt <|< sb)))
     

labcol :: Conf Label
labcol = background (id "yellow")
------------------------------------------------------------------------------------

showState :: Status -> [Label] -> Listbox [String] -> Listbox [String] -> Listbox [String] -> GUI ()
showState s ls prog heap stack =
  do ((pc,sp,fp,gp,st,hp,err),p) <- readGVar s
     cset (ls !! 1) (text (show pc))
     x <- cget (ls !! 2) text
     cset (ls !! 2) (text (show ((numval x) + 1)))
     cset (ls !! 3) (text (show sp))
     cset (ls !! 5) (text (show fp))
     cset (ls !! 7) (text (show gp))
     setValue heap (number (map show hp))
     setValue stack (number (map show st))
     setSelection prog [pc]
     cset (ls !! 9) (text (show err))
     x <- cget (ls !! 11) text
     cset (ls !! 11) (text (show ((numval x) + 1)))
     setYView prog (geNull (pc-5))
     setYView stack (geNull (sp-9))
     setYView heap (geNull ((length hp)-9))
    

geNull x = if x<0 then 0 else x

stepit :: Status -> GUI ()
stepit s = 
  do (s'@(pc,sp,fp,gp,st,hp,err),p) <- readGVar s
     if (err /= Ok) || (pc >= (length p)) 
      then tk_showError "You must reset the machine first"
      else do n'@((pc',sp',fp',gp',st',hp',err'),p') 
                   <- result (((snd (p !! pc)) s'),p)
              writeGVar s n'
              case err' of
                 Ok             -> done
                 Stopped        -> done
                 MaMaError errs -> tk_showError errs

garbage :: Status -> GUI ()
garbage s = do
  ((pc,sp,fp,gp,st,hp,err),p) <- readGVar s
  let ss = [getHeapAdr s | s <- st, isHeapAdr s]
  let ss' = ss ++ findadr hp
  let hp' = newMyHeap hp ss' 0
  writeGVar s ((pc,sp,fp,gp,st,hp',err),p)

findadr :: MyHeap -> [Int]
findadr [] = []
findadr ((Funval _ i j):xs) = [i,j] ++ findadr xs
findadr ((Closure _ j):xs) = [j] ++ findadr xs
findadr ((MaMaCons s j):xs) = [j] ++ findadr xs ++ if (isHeapAdr s) then [getHeapAdr s] else []
findadr ((Vector _ sss):xs) = [getHeapAdr s | s <- sss, isHeapAdr s] ++ findadr xs
findadr (_:xs) = findadr xs

getHeapAdr :: MaMaStackElem -> Int
getHeapAdr (HeapAdr x) = x

isHeapAdr :: MaMaStackElem -> Bool 
isHeapAdr (HeapAdr _) = True
isHeapAdr _ = False

newMyHeap :: MyHeap -> [Int] -> Int -> MyHeap
newMyHeap [] _ _ = []
newMyHeap (h:hs) ss i = if i `elem` ss then h:(newMyHeap hs ss (i+1))
                                     else Dummy:(newMyHeap hs ss (i+1))

runit :: Status -> [Label] -> Listbox [String] -> Listbox [String] -> 
         Listbox [String] -> GUI ()
runit s ls p h st = 
  do { stepit s 
     ; showState s ls p h st
     ; updateTask
     ; ((_,_,_,_,_,_,err),_) <- readGVar s
     ; if err == Ok then runit s ls p h st else done
     }

dores :: Status -> [Label] -> Listbox [String] -> Listbox [String] -> 
         Listbox [String] -> GUI ()
dores s ls p h st = 
  do { (_,fs) <- readGVar s
     ; writeGVar s (machineReset,fs)
     ; showState s ls p h st
     }

number :: [String] -> [String]
number xs = zipWith (\a b ->  (show a++": " ++ b)) [0..] xs

doload :: Edit -> GUI ()
doload e = 
  do x <- fileOpenDialogue 
     case x of 
       Nothing    -> done
       Just fname -> (setValue e (openfile fname))

docomp :: Status -> Edit -> [Label] -> Listbox [String] -> 
          Listbox [String] -> Listbox [String] -> GUI ()
docomp st e ls p h s = do
  tt <- getValue e
  let cc = linkIt (p_code (compile tt))
  let pp = parseIt cc
  setValue p (number (map fst pp))
  writeGVar st (machineReset,pp)
  showState st ls p h s

linkIt :: [String] -> [String]
linkIt xs =
  let ys = zip [0..] (map words xs)        -- give every line an address
      zs = collectLabels ys
      collectLabels [] = []
      collectLabels ((c,s):ss) =
        if ':' `elem` (head s)
        then (c,(init . head) s):collectLabels ((c,tail s):ss)
        else collectLabels ss
      ff xs = dropWhile (':' `elem`) xs
      gg (c:cs) = if c `elem` ["ldl", "ujmp", "jfalse", "mark"]
                  then [c, show (head [a | (a,l) <- zs,last cs == l])]
                  else c:cs
      yy c = c
  in map (yy . unwords . gg . ff) (map words xs)

{-
linkIt :: [String] -> [String]
linkIt xs =
  let ys = zip [0..] (map words xs)
      zs = [(c,(init . head) s) | (c,s) <- ys, ':' `elem` (head s)]
      ff (c:cs) = if ':' `elem` c then cs else c:cs
      gg (c:cs) = if c `elem` ["ldl", "ujmp", "jfalse", "mark"]
                  then [c, show (head [a | (a,l) <- zs,last cs == l])]
                  else c:cs
      yy c = c 
  in map (yy . unwords . gg . ff) (map words xs)
-}

parseIt :: [String] -> [(String,Instruction)]
parseIt = map (parseOne . words)

parseOne :: [String] -> (String,Instruction)
parseOne ("mkbasic":[]) = ("MKBASIC;",c_mkbasic)
parseOne ("mkfunval":[]) = ("MKFUNVAL;",c_mkfunval)
parseOne ("mkclos":[]) = ("MKCLOS;",c_mkclos)
parseOne ("mkvec":xs) = let n = head xs in ("MKVEC "++n++";",c_mkvec (numval n))
parseOne ("alloc":[]) = ("ALLOC;",c_alloc)
parseOne ("stop":[]) = ("STOP;",c_stop)
parseOne ("ldb":xs) = let n = head xs in ("LDB "++n++";",c_ldb (numval n))
parseOne ("getbasic":[]) = ("GETBASIC;",c_getbasic)
parseOne ("add":[]) = ("ADD;",c_add)
parseOne ("sub":[]) = ("SUB;",c_sub)
parseOne ("mult":[]) = ("MUL;",c_mul)
parseOne ("div":[]) = ("DIV;",c_div)
parseOne ("jfalse":xs) = let n = head xs in ("JFALSE "++n++";",c_jfalse (numval n))
parseOne ("ujmp":xs) = let n = head xs in ("UJUMP "++n++";",c_ujump (numval n))
parseOne ("ldl":xs) = let n = head xs in ("LDL "++n++";",c_ldl (numval n))
parseOne ("pushloc":xs) = let n = head xs in ("PUSHLOC "++n++";",c_pushloc (numval n))
parseOne ("pushglob":xs) = let n = head xs in ("PUSHGLOB "++n++";",c_pushglob (numval n))
parseOne ("mark":xs) = let n = head xs in ("MARK "++n++";",c_mark (numval n))
parseOne ("apply":[]) = ("APPLY;",c_apply)
parseOne ("targ":xs) = let n = head xs in ("TARG "++n++";",c_targ (numval n))
parseOne ("return":xs) = let n = head xs in ("RETURN "++n++";",c_return (numval n))
parseOne ("update":[]) = ("UPDATE;",c_update)
parseOne ("eval":[]) = ("EVAL;",c_eval)
parseOne ("rewrite":xs) = let n = head xs in ("REWRITE "++n++";",c_rewrite (numval n))
parseOne ("slide":xs) = let n = head xs in ("SLIDE "++n++";",c_slide (numval n))
parseOne ("cons":[]) = ("CONS;",c_cons')
parseOne ("hd":[]) = ("HD;",c_hd)
parseOne ("isnil":[]) = ("ISNIL;",c_isNil)
parseOne ("tl":[]) = ("TL;",c_tl)
parseOne ("nil":[]) = ("NIL;",c_nil)
parseOne ("eql":[]) = ("EQL;",c_equ)
parseOne ("unminus":[]) = ("UNMINUS;",c_unminus)
parseOne s = ("ERROR:"++(foldl (++) "" s)++":",c_stop)

c_isNil :: Instruction
c_isNil (pc,sp,fp,gp,st,hp,_) =
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      erg = isnil h    -- #### check error cases
      (st',err3) = setp st (sp-1)
  in (pc+1,sp,fp,gp,st'++[MaMaBasicVal (if erg==Ok then 1 else 0)],hp,err0<>err1<>err3)


