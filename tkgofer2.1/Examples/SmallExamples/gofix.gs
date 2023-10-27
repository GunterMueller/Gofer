--
-- a simple shell
--


gofix :: IO ()
gofix = start $
  do x  <- console [title "Console"] [font "12x24"] 
     st <- clipboard [initValue [""]]
     f x st where  f x st = do 
         let loop = f x st
         write x "% "
         readln  x $ \y -> do 
               case (words y) of
                 ["ls"]      -> do ls <- getValue st
                                   writeln x (unlines ls)
                                   loop
                 ("file":xs) -> do ls <- getValue st
                                   setValue st (sort (xs ++ ls))
                                   loop
                 ["help"]    -> do writeln x "available commands: file ls clear exit halt"
                                   loop 
                 ["clear"]   -> do setValue x ""
                                   loop
                 ["exit"]    -> quit
                 ["halt"]    -> writeln x "stop loop"
                 otherwise   -> do writeln x ("unknown command: " ++ y)
                                   loop

-- -------------------------------------------------------------------------- 

console :: [Conf Window] -> [Conf Edit] -> GUI Edit
console ws es = 
  do w  <- window ws 
     e <- edit ([background "white"]++es) w
     l <- vscroll [] e
     pack (flexible (flexible e <|< l))
     result e

writeln :: Edit -> String -> GUI ()
writeln e x = 
  do write e x 
     write e "\n"
     
write :: Edit -> String -> GUI ()
write e s = 
  do putEnd e s                              -- write text
     updateTask                              -- update x-events
     x <- getMark e insMark                  -- get last position 
     setYView e (fst x)                      -- scroll screen 
   
readln :: Edit -> ([Char] -> GUI ()) -> GUI ()
readln e f = 
  do focus e                                 -- display cursor
     bp <- getMark e insMark                 -- save cursor position
     cset e (on return $                     -- after return...
             do cset e (on return done)     -- no more bindings
                ep <- getMark e insMark      -- get cursor position
                x <- getFromTo e bp ep       -- read input
                f (init x)                   -- drop last newline
            )
