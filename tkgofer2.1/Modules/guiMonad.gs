----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- guiMonad.gs

-- need prelude.gs

--------------------------------------------------------------------------
-- User functions

start :: GUI a -> IO ()
start = tk_start tk_numberOfEvents

quit :: GUI ()
quit = tk_quit

liftIO :: IO a -> GUI a
liftIO = tk_action

--------------------------------------------------------------------------
-- TkGofer primitives

-- on IO

primitive primInitTcl      "primInitTcl"    :: IO Bool
primitive primWaitTcl      "primRunTcl"     :: IO ()
primitive primExecuteTcl   "primExecuteTcl" :: String -> IO String
primitive primSetVar       "primSetVar"     :: String -> IO ()
primitive primGetTcl       "primGetTcl"     :: IO Char

primUpdateEvents :: IO ()
primUpdateEvents = done -- for now; this is still a problem

primGetEvent :: IO String
primGetEvent =
  do c <- primGetTcl
     if c == '\n' then [ "" ] else [ c:s | s <- primGetEvent ]

-- on GUI

tk_getTcl :: [String] -> GUI String
tk_getTcl as = liftIO (primExecuteTcl (unwords as))
 
tk_putTcl :: [String] -> GUI ()
tk_putTcl as = void (tk_getTcl as)
 
--------------------------------------------------------------------------
-- type TkCont

data TkCont
  = TkAction    (IO TkCont)
  | TkWithTkGUI (TkGUI -> IO TkCont)
  | TkFork      TkCont TkCont
  | TkEnd
  | TkCatchEvent
  | TkQuit

tk_continue :: ((a -> TkCont) -> IO TkCont) -> GUI a
tk_continue f cont = TkAction (f cont)

tk_withCont :: ((a -> TkCont) -> GUI TkCont) -> GUI a
tk_withCont f cont = f cont id

tk_GUI :: TkCont -> GUI ()
tk_GUI cont _ = cont

tk_action :: IO a -> GUI a
tk_action io = tk_continue (`map` io)

tk_withTkGUI :: (TkGUI -> IO a) -> GUI a
tk_withTkGUI io cont = TkWithTkGUI (map cont . io)

tk_fork :: GUI a -> GUI ()
tk_fork gui cont = TkFork (gui (\_ -> TkEnd)) (cont ())

tk_quit :: GUI a
tk_quit cont = TkQuit

tk_end :: GUI a
tk_end cont = TkEnd

tk_transGUI :: (TkCont -> TkCont) -> GUI a -> GUI a
tk_transGUI f gui cont = f (gui cont)

tk_melt :: GUI a -> GUI a
tk_melt gui = tk_transGUI melt (do { a <- gui; tk_fork done; result a })
 where
  melt (TkAction io)   = melt (TkWithTkGUI (const io))
  melt (TkWithTkGUI f) = TkWithTkGUI $ \tkgui ->
                           do ac <- f tkgui
                              case melt ac of
                                TkAction io   -> io
                                TkWithTkGUI f -> f tkgui
                                act           -> result act
  melt act             = act

instance Text TkCont where
  showsPrec _ x = showString $ case x of
    TkAction io   -> "Action {" ++ take 20 (show' io) ++ "..}"
    TkWithTkGUI f -> "WithGUI {" ++ take 20 (show' f) ++ "..}"
    TkFork p1 p2  -> "Fork {" ++ show p1 ++ " | " ++ show p2 ++ "}"
    TkEnd         -> "End"
    TkQuit        -> "Quit"
    TkCatchEvent  -> "CatchEvent"

--------------------------------------------------------------------------
-- type GUI

type TkGUI = (TkCallBackS, Var Int)

type GUI a = (a -> TkCont) -> TkCont
  in tk_bindGUI, tk_resultGUI, tk_runGUI
   , tk_continue, tk_withCont, tk_withTkGUI, tk_fork, tk_quit, tk_end
   , tk_GUI, tk_transGUI

instance Monad GUI where
  bind   = tk_bindGUI
  result = tk_resultGUI

instance Functor GUI where
  map f m = [ f x | x <- m ]

tk_bindGUI   :: GUI a -> (a -> GUI b) -> GUI b
tk_resultGUI :: a -> GUI a

m `tk_bindGUI` k = \cont -> m (\a -> k a cont)
tk_resultGUI a   = \cont -> cont a

instance Monad0 GUI where
  zero = error "Pattern match failed in GUI-monad."

instance IOMonad GUI where
  putStr s    = liftIO (putStr s)
  getCh       = liftIO getCh
  getChar     = liftIO getChar
  getContents = liftIO getContents

interleaveGUI :: GUI a -> GUI a
interleaveGUI gui = tk_withTkGUI $ \tkgui -> interleaveIO $
  do res <- newVar (error "interleaveGUI too strict")
     tk_runGUI tkgui $ do { a <- gui ; writeGVar res a ; tk_quit }
     readVar res
     
--------------------------------------------------------------------------
-- running a GUI

-- start

tk_start :: Int -> GUI a -> IO ()
tk_start n gui =
  do cbs <- tk_callBackS n
     var <- newVar 0
     b   <- primInitTcl
     if b then tk_runGUI (cbs, var) gui
          else error "Initialization error."
  
-- run

tk_runGUI :: TkGUI -> GUI a -> IO ()
tk_runGUI tkgui@(callbacks, names) gui =
  round [gui (\_ -> TkCatchEvent)]
 where
  round :: [TkCont] -> IO ()
  round []     = error "Deadlock"
  round (r:rs) = case r of
    TkAction io   -> do pc <- io
                        postpone [pc]

    TkWithTkGUI f -> do pc <- f tkgui
                        postpone [pc]

    TkFork p1 p2  -> postpone [p1,p2]

    TkEnd         -> round rs

    TkQuit        -> done

    TkCatchEvent  -> do if null rs then
                          primWaitTcl
                         else
                          primUpdateEvents
                        pc <- catchEvent
                        postpone [pc]

   where
    postpone ps = round (rs ++ ps)
    
  catchEvent =
    do ev <- primGetEvent
       case words ev of
         []      -> do result TkCatchEvent
         ("q":_) -> do result TkQuit
         (e:es)  -> do gui <- tk_getCallBack callbacks (readPtr e) es
                       result (gui (\_ -> TkCatchEvent))

--------------------------------------------------------------------------
-- GVar

type GVar = Var
--in newGVar, newGVar', writeGVar, readGVar

newGVar :: a -> GUI (GVar a)
newGVar a = liftIO (newVar a)

newGVar' :: GUI (GVar a)
newGVar' = liftIO (newVar')

writeGVar :: GVar a -> a -> GUI ()
writeGVar v a = liftIO (writeVar v a)

readGVar :: GVar a -> GUI a
readGVar v = liftIO (readVar v)

updateGVar :: (a -> a) -> GVar a -> GUI ()
updateGVar f v =
  do a <- readGVar v
     writeGVar v (f a)

fixGUI :: (GUI a -> GUI a) -> GUI a
fixGUI f =
  do v <- newGVar'
     a <- f (readGVar v)
     writeGVar v a
     result a

-- Class IsVar

class IsVar v where
  newV :: a -> GUI (v a)
  newV a = do v <- newV'; v =: a; result v

  newV' :: GUI (v a)
  newV' = do v <- newV undefined; readV v; result v

  writeV :: v a -> a -> GUI ()
  readV  :: v a -> GUI a

  (=:) :: v a -> a -> GUI ()
  (=:) = writeV

  updateV :: (a -> a) -> v a -> GUI ()
  updateV f v = do a <- readV v; v =: (f a)

instance IsVar GVar where
  newV   = newGVar
  newV'  = newGVar'
  readV  = readGVar
  writeV = writeGVar

--------------------------------------------------------------------------
-- Identifiers

type TkWidId = String

tk_newPathName :: GUI TkWidId
tk_newPathName = tk_withTkGUI $ \(_, name) ->
  do n <- readVar name
     let m = n+1
     writeVar name m
     strict result (".w" ++ show m)

--------------------------------------------------------------------------
-- CallBacks

type TkCallBack = (TkWidId, [String] -> GUI ())
  in tk_callBackS, tk_getCallBack, tk_addCallBack
   , tk_delEventId, tk_delThese, tk_delEvents, tk_delBelowEvents
   , tk_changeCallBack

type TkCallBackS = Heap TkCallBack
  in tk_callBackS, tk_getCallBack, tk_addCallBack
   , tk_delEventId, tk_delThese, tk_delEvents, tk_delBelowEvents
   , tk_delCallBack, tk_changeCallBack

tk_numberOfEvents = 5000

-- on IO

tk_callBackS :: Int -> IO TkCallBackS
tk_callBackS n =
  do newHeap n

tk_getCallBack :: TkCallBackS -> HeapPtr -> [String] -> IO (GUI ())
tk_getCallBack heap ptr xs =
  do (_, cb) <- heapRead heap ptr
     result (cb xs)

-- on GUI
    
tk_withCallBackS :: (TkCallBackS -> IO a) -> GUI a
tk_withCallBackS f = tk_withTkGUI (\(cbs,_) -> f cbs)

-- widgets

tk_addCallBack :: TkWidId -> ([String] -> GUI ()) -> GUI HeapPtr
tk_addCallBack name f = tk_withCallBackS $ \heap ->
  do heapAlloc heap (name++".", f)

tk_changeCallBack :: HeapPtr -> ([String] -> GUI ()) -> GUI ()
tk_changeCallBack p f = tk_withCallBackS $ \heap ->
  do (name,_) <- heapRead heap p
     heapWrite heap p (name,f)

tk_delThese :: TkWidId -> GUI ()
tk_delThese name = tk_withCallBackS $ \heap ->
  do heapFilter heap $ \(s, _) -> not (name++"." == s)

tk_delEvents :: TkWidId -> GUI ()
tk_delEvents name = tk_withCallBackS $ \heap ->
  do heapFilter heap $ \(s, _) -> not (name++"." `isPrefix` s)

tk_delBelowEvents :: TkWidId -> GUI ()
tk_delBelowEvents name = tk_withCallBackS $ \heap ->
  do heapFilter heap $ \(s, _) -> not (name++"." `isRealPrefix` s)

-- events

tk_delCallBack :: HeapPtr -> GUI ()
tk_delCallBack p = tk_withCallBackS $ \heap ->
  do heapFree heap p

tk_delEventId :: String -> GUI ()
tk_delEventId "" = done
tk_delEventId s  = tk_delCallBack . readPtr . head . words $ s

--------------------------------------------------------------------------
-- the end.
            
