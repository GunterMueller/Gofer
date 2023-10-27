----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- concurrent.gs

-- need prelude.gs
-- need guiMonad.gs

--------------------------------------------------------------------------
-- Fork

fork :: GUI a -> GUI ()
fork = tk_fork

forks :: [GUI a] -> GUI ()
forks = seqs . map fork

--------------------------------------------------------------------------
-- Atomic

-- make sure that the atomic action terminates!

atomic :: GUI a -> GUI a
atomic = tk_melt

--------------------------------------------------------------------------
-- Derived

par :: GUI a -> GUI b -> GUI (a,b)
par p1 p2 =
  do v <- newMVar'
     fork (p1 `bind` putMVar v)
     b <- p2
     a <- takeMVar v
     result (a,b)

choice :: [(MVar a, a -> GUI b)] -> GUI b
choice cs =
  do won <- newMVar True
     ans <- newMVar'
     forks [ wait won ans p | p <- cs ]
     takeMVar ans
 where
  wait won ans (v,f) =
    do a <- takeMVar v
       w <- takeMVar won
       putMVar won False
       if w then do
         c <- f a
         putMVar ans c
        else
         putMVar v a

--------------------------------------------------------------------------
-- MVar's

type MVar a = Var (Either a [a -> TkCont])
  in newMVar', newMVar, takeMVar, putMVar, eqMVar

newMVar' :: GUI (MVar a)
newMVar' = tk_action $
  do newVar (Right [])

newMVar :: a -> GUI (MVar a)
newMVar a = tk_action $
  do newVar (Left a)

takeMVar :: MVar a -> GUI a
takeMVar mvar = tk_continue $ \cont ->
  do ma <- readVar mvar
     case ma of
       Left a   -> do writeVar mvar (Right [])
                      result (cont a)
       Right ws -> do writeVar mvar (Right (ws++[cont]))
                      result TkEnd

putMVar :: MVar a -> a -> GUI ()
putMVar mvar a = tk_continue $ \cont ->
  do ma <- readVar mvar
     case ma of
       Right []     -> do writeVar mvar (Left a)
                          result (cont ())
       Right (w:ws) -> do writeVar mvar (Right ws)
                          result (TkFork (cont ()) (w a))
       Left a       -> error "putMVar on a full MVar"

updateMVar :: (a -> a) -> MVar a -> GUI ()
updateMVar f mvar =
  do a <- takeMVar mvar
     putMVar mvar (f a)

eqMVar :: MVar a -> MVar a -> Bool
eqMVar = eqVar

instance Eq (MVar a) where
  (==) = eqMVar

instance IsVar MVar where
  newV   = newMVar
  newV'  = newMVar'
  readV  = takeMVar
  writeV = putMVar

--------------------------------------------------------------------------
-- CVar's

type CVar a = (MVar (), MVar a)
  in newCVar', newCVar, takeCVar, putCVar, eqCVar

newCVar' :: GUI (CVar a)
newCVar' =
  do vu <- newMVar ()
     va <- newMVar'
     result (vu,va)

newCVar :: a -> GUI (CVar a)
newCVar a =
  do vu <- newMVar'
     va <- newMVar a
     result (vu,va)

takeCVar :: CVar a -> GUI (a)
takeCVar (vu,va) =
  do a <- takeMVar va
     putMVar vu ()
     result a

putCVar :: CVar a -> a -> GUI ()
putCVar (vu,va) a =
  do takeMVar vu
     putMVar va a

eqCVar :: CVar a -> CVar a -> Bool
eqCVar (v1,v2) (w1,w2) = v1 `eqMVar` w1 && v2 `eqMVar` w2

instance Eq (CVar a) where
  (==) = eqCVar

instance IsVar CVar where
  newV   = newCVar
  newV'  = newCVar'
  readV  = takeCVar
  writeV = putCVar

--------------------------------------------------------------------------
-- Channels

type Channel a = (MVar (ChanStr a), MVar (ChanStr a))
  in newChan', putChan, getChan, eqChan

data ChanStr a = ChanStr (MVar (a, ChanStr a))

newChan' :: GUI (Channel a)
newChan' =
  do hole <- newMVar'
     let chole = ChanStr hole
     read  <- newMVar chole
     write <- newMVar chole
     result (read,write)

putChan :: Channel a -> a -> GUI ()
putChan (read,write) a =
  do hole' <- newMVar'
     let chole' = ChanStr hole'
     (ChanStr hole) <- takeMVar write
     putMVar write chole'
     putMVar hole (a, chole')

getChan :: Channel a -> GUI a
getChan (read,write) =
  do (ChanStr cts) <- takeMVar read
     (a,new) <- takeMVar cts
     putMVar read new
     result a

eqChan :: Channel a -> Channel a -> Bool
eqChan (v1,v2) (w1,w2) = v1 `eqMVar` w1 && v2 `eqMVar` w2

instance Eq (Channel a) where
  (==) = eqChan

instance IsVar Channel where
  newV'  = newChan'
  readV  = getChan
  writeV = putChan

newChan :: a -> GUI (Channel a)
newChan = newV

--------------------------------------------------------------------------
-- SkipChan's

type SkipChan a = (MVar (a, [MVar ()]), MVar ())
  in newSkipChan', putSkipChan, getSkipChan, dupSkipChan, eqSkipChan

newSkipChan' :: GUI (SkipChan a)
newSkipChan' =
  do sem  <- newMVar'
     main <- newMVar (undefined, [sem])
     result (main, sem)

putSkipChan :: SkipChan a -> a -> GUI ()
putSkipChan (main,_) v =
  do (_,sems) <- takeMVar main
     putMVar main (v,[])
     seqs [ putMVar sem () | sem <- sems ]

getSkipChan :: SkipChan a -> GUI a
getSkipChan (main,sem) =
  do takeMVar sem
     (v,sems) <- takeMVar main
     putMVar main (v,sem:sems)
     result v

dupSkipChan :: SkipChan a -> GUI (SkipChan a)
dupSkipChan (main,_) =
  do sem <- newMVar'
     (v,sems) <- takeMVar main
     putMVar main (v, sem:sems)
     result (main, sem)

eqSkipChan :: SkipChan a -> SkipChan a -> Bool
eqSkipChan (v1,v2) (w1,w2) = v1 `eqMVar` w1 && v2 `eqMVar` w2

instance Eq (SkipChan a) where
  (==) = eqSkipChan

instance IsVar SkipChan where
  newV'  = newSkipChan'
  readV  = getSkipChan
  writeV = putSkipChan

----------------------------------------------------------------
-- the end.

