------------------------------------------------------
-- Killable processes

-- extension to fork: Process Id's and kill.

forkPid :: GUI a -> GUI Pid
forkPid = tk_forkPid

kill :: Pid -> GUI ()
kill = tk_kill

-- implementation

type Pid = GVar Bool
  in tk_forkPid, tk_kill, tk_eqPid

instance Eq Pid where
  (==) = tk_eqPid

tk_eqPid :: Pid -> Pid -> Bool
tk_eqPid = (==)

tk_forkPid :: GUI a -> GUI Pid
tk_forkPid gui =
  do run <- newGVar True
     fork (checks run $ do { gui; tk_end })
     result run
 where
  checks run = tk_transGUI check
   where
    check (TkAction io)   = check (TkWithTkGUI (const io))
    check (TkWithTkGUI f) = TkWithTkGUI $ \tkgui ->
                              do b <- readVar run
                                 if b then map check (f tkgui)
                                      else result TkEnd
                                              
    check (TkFork p1 p2)  = TkFork (check p1) (check p2)
    check p               = p

tk_kill :: Pid -> GUI ()
tk_kill run = writeGVar run False

------------------------------------------------------
-- the end.


