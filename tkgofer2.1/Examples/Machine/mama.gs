----------------------------------------------------------------------------
-- MaMa the machine
-- 12/95 by Thilo Schwinn
-- Abteilung Programmiermethodik
-- Universitaet Ulm
----------------------------------------------------------------------------

-- Type Definitions --------------------------------------------------------

data MaMaStackElem = MaMaBasicVal Int
               | MaMaStackAdr Int
               | HeapAdr Int
               | ProgAdr Int
type MaMaStack     = [MaMaStackElem]
data MyHeapElem  = MaMaBasic Int 
               | Funval Int Int Int
               | Closure Int Int 
               | Vector Int [MaMaStackElem]
               | MaMaCons MaMaStackElem Int     
               | Nil
               | Dummy

type MyHeap      = [MyHeapElem]
type Machine   = (Int,Int,Int,Int,MaMaStack,MyHeap,Flag)
--                PC  SP  FP  GP  ST    HP   Machine status

machineReset = (0,nil,nil,nil,[],[],Ok)

data Flag = Ok | Stopped | MaMaError String

instance Eq Flag where
  Ok == Ok = True
  Stopped == Stopped = True
  _ == _ = False

instance Text Flag where
  showsPrec d Ok      = showString "Ok"
  showsPrec d Stopped = showString "Stp"
  showsPrec d _       = showString "Err"

type Instruction = Machine -> Machine

instance Text MaMaStackElem where
  showsPrec d (MaMaBasicVal i) = showString "value " . shows i
  showsPrec d (MaMaStackAdr i) = showString "stack " . shows i
  showsPrec d (HeapAdr i) = showString "heap " . shows i
  showsPrec d (ProgAdr i) = showString "code " . shows i
 
instance GUIValue MaMaStackElem where
  tk_defaultValue = MaMaBasicVal 0

instance Text MyHeapElem where
  showsPrec d (MaMaBasic i) = showString "bas (".shows i.showChar ')'
  showsPrec d (Funval a b c) =
    showString ("fun (cf "++show a++", fap "++show b++", fgp "++show c++")")
  showsPrec d (Closure a b) =
    showString ("clos (cp "++show a++", gp "++show b++")")
  showsPrec d (Vector s vs) =
   showString ("vec ("++list vs++")")
     where list [] = ""
           list xs = init (concat (map (\x -> show x++",") xs))
  showsPrec d Nil = showString "NIL"
  showsPrec d Dummy = showString "--"
  showsPrec d (MaMaCons a b) =
    showString ("CONS head "++show a++": tail "++show b)

instance GUIValue MyHeapElem where
  tk_defaultValue = MaMaBasic 0

----------------------------------------------------------------------------

infixl 7 <>                      -- machine status 'and'
(<>) :: Flag -> Flag -> Flag
Ok <> Ok = Ok
(MaMaError e) <> Ok = MaMaError e
Ok <> (MaMaError e) = MaMaError e
(MaMaError e) <> (MaMaError f) = MaMaError (e++f)
_ <> _ = MaMaError "machine stopped"

---------------------------------------------------------------------------

class GUIValue a => Stores a where

  load   :: [a] -> Int -> (a,Flag)
  store  :: [a] -> Int -> a -> ([a],Flag)
  store xs i x = let (p1,err1) = fromto xs 0 (i-1)
                     (p2,err2) = fromto xs (i+1) ((length xs)-1)
                 in (p1++x:p2,err1<>err2)
  setp   :: [a] -> Int -> ([a],Flag)
  setp xs i = fromto xs 0 i
  fromto :: [a] -> Int -> Int -> ([a],Flag)
  fromto xs a b = 
    if a<=b 
    then
      let erg = [ fst (load xs y) | y <- [a..b]]
          err = foldr1 (<>) [ snd (load xs y) | y <- [a..b]]
      in (erg,err)
    else ([],Ok)
        

instance Stores MaMaStackElem where

  load xs i = if (i<0) || (i>=(length xs)) 
              then (tk_defaultValue,MaMaError ("Out of stack range: "++ show i))
              else (xs !! i,Ok)

instance Stores MyHeapElem where

  load xs i = if (i<0) || (i>=(length xs))
              then (tk_defaultValue,MaMaError "Out of heap range")
              else (xs !! i,Ok)

nil = -1

-- get stack and heap elements 

bv :: MaMaStackElem -> (Int,Flag)
bv (MaMaBasicVal x) = (x,Ok)
bv e = (0, MaMaError ("No basic Value: "++show e))

pa :: MaMaStackElem -> (Int,Flag)
pa (ProgAdr x) = (x,Ok)
pa e = (0, MaMaError ("No program address: "++show e))

sa :: MaMaStackElem -> (Int,Flag)
sa (MaMaStackAdr x) = (x,Ok)
sa e = (0, MaMaError ("No stack address: "++show e))

ha :: MaMaStackElem -> (Int,Flag)
ha (HeapAdr x) = (x,Ok)
ha e = (0, MaMaError ("No heap address: "++show e))

ldst :: MaMaStack -> Int -> (MaMaStackElem -> (Int,Flag)) -> (Int,Flag)
ldst st i f = let (erg,err) = load st i
              in if err==Ok then f erg else (0,err)

basic :: MyHeapElem -> (Int,Flag)
basic (MaMaBasic x) = (x,Ok)
basic e = (0, MaMaError ("No basic: "++show e))

funval :: MyHeapElem -> (Int,Int,Int,Flag)
funval (Funval a b c) = (a,b,c,Ok)
funval e = (0,0,0, MaMaError ("No funval: "++show e))

closure :: MyHeapElem -> (Int,Int,Flag)
closure (Closure a b) = (a,b,Ok)
closure e = (0,0, MaMaError ("No closure: "++show e))

vector :: MyHeapElem -> (Int,[MaMaStackElem],Flag)
vector (Vector s vs) = (s,vs,Ok)
vector e = (0,[], MaMaError ("No vector: "++show e))

cons :: MyHeapElem -> (MaMaStackElem,Int,Flag)
cons (MaMaCons a b) = (a,b,Ok)
cons e = (MaMaBasicVal 0,0, MaMaError ("No cons: "++show e))

isnil :: MyHeapElem -> Flag
isnil Nil = Ok
isnil e = MaMaError ("Not nil: "++show e)

------------------------------------------------------------------------

-- instructions for heap allocation

c_mkbasic :: Instruction
c_mkbasic (pc,sp,fp,gp,st,hp,_) = 
  let new = length hp
      (s0,err0) = ldst st sp bv 
      (st',err1) = setp st (sp-1)
  in (pc+1,sp,fp,gp,st'++[HeapAdr new],hp ++ [(MaMaBasic s0)],err0<>err1)

c_mkfunval :: Instruction
c_mkfunval (pc,sp,fp,gp,st,hp,_) = 
  let new = length hp
      (s0,err0) = ldst st sp pa 
      (s1,err1) = ldst st (sp-1) ha
      (s2,err2) = ldst st (sp-2) ha 
      (st',err3) = setp st (sp-3)
      err = err0 <> err1 <> err2 <> err3
  in (pc+1,sp-2,fp,gp,st'++[HeapAdr new],hp ++ [(Funval s0 s1 s2)],err)

c_mkclos :: Instruction
c_mkclos (pc,sp,fp,gp,st,hp,_) = 
  let new = length hp
      (s0,err0) = ldst st sp pa 
      (s1,err1) = ldst st (sp-1) ha 
      (st',err2) = setp st (sp-2)
  in (pc+1,sp-1,fp,gp,st'++[HeapAdr new]
     ,hp ++ [(Closure s0 s1)],err0 <> err1 <> err2)

c_mkvec :: Int -> Instruction
c_mkvec n (pc,sp,fp,gp,st,hp,_) = 
  let new  = length hp
      (args,err0)  = fromto st (sp-n+1) sp
      (st',err1)   = setp st (sp-n)
  in (pc+1,sp-n+1,fp,gp,st'++[HeapAdr new],hp ++ [(Vector n args)],err0 <> err1)

c_alloc :: Instruction
c_alloc (pc,sp,fp,gp,st,hp,_) = 
  let new = length hp
  in (pc+1,sp+1,fp,gp,st++[HeapAdr new],hp ++ [(Closure nil nil)],Ok)

-- stop

c_stop :: Instruction
c_stop (pc,sp,fp,gp,st,hp,_) = (pc,sp,fp,gp,st,hp,Stopped)

-- basic instructions, branches

c_ldb :: Int -> Instruction
c_ldb b (pc,sp,fp,gp,st,hp,_) = (pc+1,sp+1,fp,gp,st++[MaMaBasicVal b],hp,Ok)

c_getbasic :: Instruction
c_getbasic (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (x,err1)  = load hp s0
      (x',err2) = basic x 
      (st',err3) = setp st (sp-1)
  in (pc+1,sp,fp,gp,st'++[MaMaBasicVal x'],hp,err0<>err1<>err2<>err3)

c_op :: (Int -> Int -> Int) -> Instruction
c_op f (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp bv 
      (s1,err1) = ldst st (sp-1) bv 
      (st',err2) = setp st (sp-2)
  in (pc+1,sp-1,fp,gp,st'++[MaMaBasicVal (f s1 s0)],hp,err0 <> err1 <> err2)

c_add = c_op (+)
c_sub = c_op (-)
c_mul = c_op (*)
c_div = c_op (/)
c_equ = c_op (\x y -> if x==y then 1 else 0)

c_unminus :: Instruction
c_unminus (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp bv 
      (st',err1) = setp st (sp-1)
  in (pc+1,sp-1,fp,gp,st'++[MaMaBasicVal (-s0)],hp,err0<>err1)

c_jfalse :: Int -> Instruction
c_jfalse l (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp bv
      pc' = if s0 == 0 then l else pc +1
      (st',err1) = setp st (sp-1)
  in (pc',sp-1,fp,gp,st',hp,err0<>err1)

c_ujump :: Int -> Instruction
c_ujump l (pc,sp,fp,gp,st,hp,_) = (l,sp,fp,gp,st,hp,Ok) 


c_ldl :: Int -> Instruction
c_ldl l (pc,sp,fp,gp,st,hp,_) = (pc+1,sp+1,fp,gp,st++[ProgAdr l],hp,Ok)

-- stack operations

c_pushloc :: Int -> Instruction
c_pushloc j (pc,sp,fp,gp,st,hp,_) = 
  let (s,err) = load st (sp+1-j) 
  in (pc+1,sp+1,fp,gp,st++[s],hp,err)

c_pushglob :: Int -> Instruction
c_pushglob j (pc,sp,fp,gp,st,hp,_) = 
  let (h,err0) = load hp gp
      (_,vs,err1) = vector h
      (x,err2)  = load vs (j-1)   -- #### vector start with count 1
  in (pc+1,sp+1,fp,gp,st++[x],hp,err0<>err1<>err2)

-- create stack frame

c_mark :: Int -> Instruction
c_mark l (pc,sp,fp,gp,st,hp,_) 
  = (pc+1,sp+3,sp+3,gp,st++[ProgAdr l,MaMaStackAdr fp,HeapAdr gp],hp,Ok) 

-- function application

c_apply :: Instruction
c_apply (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      (cf,fap,fgp,err2) = funval h
      (h2,err3) = load hp fap
      (s,vs,err4) = vector h2
      (st',err5) = setp st (sp-1)
  in (cf,sp-1+s,fp,fgp,st'++(reverse vs),hp,err0<>err1<>err2<>err3<>err4<>err5) 

-- test arguments

c_targ :: Int -> Instruction
c_targ n (pc,sp,fp,gp,st,hp,_) = 
  if (sp-fp)>=n 
  then (pc+1,sp,fp,gp,st,hp,Ok) 
  else let (pc',err0) = ldst st (fp-2) pa
           new1 = length hp
           (hh,err1) = fromto st (fp+1) sp
           elem1 = Vector (length hh) hh
           new2 = new1+1
           elem2 = Funval pc new1 gp
           (st',err2) = setp st (fp-3)
           (gp',err3) = ldst st fp ha
           (fp',err4) = ldst st (fp-1) sa
       in (pc',fp-2,fp',gp',st'++[HeapAdr new2]
          ,hp++[elem1,elem2],err0<>err1<>err2<>err3<>err4) 


-- return from function application

c_return :: Int -> Instruction
c_return n (pc,sp,fp,gp,st,hp,_) =
  if sp == (fp +1 +n) 
  then let (pc',err0) = ldst st (fp-2) pa
           (fp',err1) = ldst st (fp-1) sa
           (gp',err2) = ldst st fp ha
           (st',err3) = setp st (fp-3)
           (x,err4)   = load st sp 
       in (pc',fp-2,fp',gp',st'++[x],hp,err0<>err1<>err2<>err3<>err4)
  else let (s0,err0) = ldst st sp ha
           (h,err1) = load hp s0
           (cf,fap,fgp,err2) = funval h
           (h2,err3) = load hp fap
           (s,vs,err4) = vector h2
           (st',err5) = setp st (sp-n-1)
       in (cf,sp-n-1+s,fp,fgp,st'++(reverse vs)
          ,hp,err0<>err1<>err2<>err3<>err4<>err5)

-- misc.

c_update :: Instruction
c_update (pc,sp,fp,gp,st,hp,_) =
  let (pc',err0) = ldst st (fp -2) pa
      (gp',err1) = ldst st fp ha
      sp' = fp-3
      (fp',err2) = ldst st (fp-1) sa
      (st',err3) = setp st (fp-3)
      (s0,err4) = ldst st sp ha
      (h,err5) = load hp s0
      (s4,err6) = ldst st (sp-4) ha
      (hp',err7) = store hp s4 h
  in (pc',sp',fp',gp',st',hp',err0<>err1<>err2<>err3<>err4<>err5<>err6<>err7)


c_eval :: Instruction
c_eval (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      (cp,cgp,isclos) = closure h
  in if isclos == Ok 
     then (cp,sp+3,sp+3,cgp,st++[ProgAdr pc,MaMaStackAdr fp,HeapAdr gp],hp,err0<>err1)
     else (pc+1,sp,fp,gp,st,hp,err0<>err1)

c_rewrite :: Int -> Instruction
c_rewrite m (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (sm,err1) = ldst st (sp-m) ha
      (h,err2) = load hp s0
      (hp',err3) = store hp sm h
      (st',err4) = setp st (sp-1)
  in (pc+1,sp-1,fp,gp,st',hp',err0<>err1<>err2<>err3<>err4)

c_slide :: Int -> Instruction
c_slide m (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = load st sp
      (st',err1) = store st (sp-m) s0
      (st'',err2) = setp st' (sp-m)
  in (pc+1,sp-m,fp,gp,st'',hp,err0<>err1<>err2)

-- list operations

c_cons :: Instruction
c_cons (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = load st sp 
      (s1,err1) = load st (sp-1) 
      (s1',err2) = ha s1
      (h,err3) = load hp s1'
      (hd,tl,ic) = cons h
      isn = isnil h
      new = length hp
      (st',err4) = setp st (sp-2)
      err=err0<>err1<>err2<>err3<>err4
  in if (isn == Ok) || (ic == Ok) 
     then (pc+1,sp-1,fp,gp,st'++[HeapAdr new],hp++[MaMaCons s0 s1'],err)
     else (pc+1,sp,fp,gp,st,hp,err<>isn<>ic)

c_cons' :: Instruction
c_cons' (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = load st sp 
      (s1,err1) = load st (sp-1) 
      (s1',err2) = ha s1
      (h,err3) = load hp s1'
      (hd,tl,ic) = cons h
      isn = isnil h
      (_,_,isclos) = closure h
      (_,_,_,isfun) = funval h
      new = length hp
      (st',err4) = setp st (sp-2)
      err=err0<>err1<>err2<>err3<>err4
  in if (isn == Ok) || (ic == Ok) || (isclos == Ok) || (isfun == Ok) 
     then (pc+1,sp-1,fp,gp,st'++[HeapAdr new],hp++[MaMaCons s0 s1'],err)
     else (pc+1,sp,fp,gp,st,hp,err<>isn<>ic)


c_hd :: Instruction
c_hd (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      (hd,_,err2) = cons h
      (st',err3) = setp st (sp-1)
  in (pc+1,sp,fp,gp,st'++[hd],hp,err0<>err1<>err2<>err3)

c_tl :: Instruction
c_tl (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      (_,tl,err2) = cons h
      (st',err3) = setp st (sp-1)
  in (pc+1,sp,fp,gp,st'++[HeapAdr tl],hp,err0<>err1<>err2<>err3)

c_tl' :: Instruction
c_tl' (pc,sp,fp,gp,st,hp,_) = 
  let (s0,err0) = ldst st sp ha
      (h,err1) = load hp s0
      (_,tl,err2) = cons h
      (st',err3) = setp st (sp-1)
  in cfunclos (pc+1,sp,fp,gp,st'++[HeapAdr tl],hp,err0<>err1<>err2<>err3)

cfunclos :: Instruction
cfunclos (pc,sp,fp,gp,st,hp,i) = 
  let (s0,_) = ldst st sp ha
      (h,_)  = load hp s0
      (_,_,isclos)  = closure h
      (_,_,_,isfun) = funval h
  in if isclos == Ok then cfunclos (c_eval (pc,sp,fp,gp,st,hp,i))
                     else if isfun == Ok then (c_apply (pc,sp,fp,gp,st,hp,i))
                                         else (pc,sp,fp,gp,st,hp,i)

c_nil :: Instruction
c_nil (pc,sp,fp,gp,st,hp,_) = 
  let new = length hp
  in (pc+1,sp+1,fp,gp,st++[HeapAdr new],hp++[Nil],Ok)





-------------------------------------------------------------------------



