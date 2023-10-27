----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- time.gs

-- need prelude.gs
-- need guiMonad.gs
-- need widget.gs

----------------------------------------------------------------
-- time & waiting
 
after :: Int -> GUI a -> GUI ()
after n action =
  do p <- tk_addCallBack "after" undefined
     tk_changeCallBack p $ \_ ->
       do action
          tk_delCallBack p
 
     tk_putTcl ["after", show n, "doEvent", show p ]
 
delay :: Int -> GUI ()
delay n = tk_withCont $ \cont ->
  do after n (tk_fork $ tk_GUI $ cont ())
     result TkEnd
 
every :: Int -> GUI a -> GUI ()
every i a = do a; after i (every i a)

----------------------------------------------------------------
-- return ellapsed time : actual value depends on system !!!

getTicks :: GUI Int
getTicks = map ((/1000) . numval) (tk_getTcl ["clock clicks"])

-- a very bad random function

random :: Int -> GUI Int
random n = map (f.numval) (tk_getTcl ["clock clicks"])
 where
  f t = (t*t) `mod` n

-- relative measurement of elapsed time

timeOf :: GUI a -> GUI Int
timeOf f
  = do startT <- getTicks
       f 
       endT <- getTicks
       result (endT - startT)
 
----------------------------------------------------------------
-- Timer Widget

data Timer0 a = Timer0 (Var (a, GUI (), Bool))
 
type Timer = TItem (Timer0 Int)
 
timer :: [Conf Timer] -> GUI Timer
timer cs =
  do v <- newGVar (1000, done, False)
     i <- tk_newPathName
     let c = TItem (Timer0 v) i
     csets c $ if elemAct (map ($c) cs)
                 then cs ++ [active True]
                 else cs
     result c
 where
  elemAct []               = True
  elemAct (Tk_Active _:cs) = False
  elemAct (_:cs)           = elemAct cs
 
runTimer :: Timer -> GUI ()
runTimer timer@(TItem (Timer0 v) i) =
  do (t,c,b) <- readGVar v
     if b then do
       after t (runTimer timer)
       c
      else
       done
 
instance HasCommand (TItem (Timer0 Int))
  => HasInput TItem Timer0 Int where
 
  getValue (TItem (Timer0 v) _) =
    [ t | (t,_,_) <- readGVar v ]
 
  setValue c@(TItem (Timer0 v) _) t =
    updateGVar (\(_,c,b) -> (max t 10,c,b)) v
 
  readOnly _ _  = Tk_DummyConf
 
instance Widget Timer where
  cset w@(TItem (Timer0 v) i) c =
    let newc = c w
    in case newc of
         Tk_Command a -> updateGVar (\(t,_,b) -> (t,a,b)) v
         Tk_Active b' ->
           do (t,c,b) <- tk_melt $ do (t,c,b) <- readGVar v
                                      writeGVar v (t,c,b')
                                      result (t,c,b)
              if b' && not b then
                runTimer w
               else done
         otherwise    -> void $ tk_showConf i (c w)
 
  cget  w@(TItem (Timer0 v) i) f =
    let c = f tk_defaultValue w in case c of
      Tk_Active _ -> [ typeCast b | (_,_,b) <- readGVar v ]
      otherwise   -> result tk_defaultValue
 
  onArgs _ _ _ _ = Tk_DummyConf
 
instance HasCommand Timer

--------------------------------------------------------------------------
-- the end.

