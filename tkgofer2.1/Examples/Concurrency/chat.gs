--------------------------------------------------
-- chat.gs, a very simple chatbox.
-- Koen Claessen, 1997
--------------------------------------------------

-- This file must be readable and writable for all chatters

chatFile :: FilePath
chatFile = "/users/cs/koen/Chat/chat.txt"

chatFont :: FilePath
chatFont = "7x13"

-- main

main :: IO ()
main = start $
  do w <- window [ title "Name?" ]
     i <- entry  [ initValue "No-one", self (on return . ready w) ] w
     b <- button [ text "Ok", command (ready w i) ] w
     pack (i <|< b)
 where
  ready w i =
    do name <- getValue i
       closeWindow w
       chat name

chat :: String -> GUI ()
chat name =
  do send ("-- " ++ name ++ " starts chatting --")
     w <- window  [ title "Chatting" ]
     e <- edit    [ width 80, height 30, font chatFont ] w
     s <- vscroll [ ] e
     i <- entry   [ initValue "", font chatFont ] w
     b <- button  [ text "Quit", command stop ] w
     pack $ (e <|< s) ^-^ (flexible i <|< b)

     doOutput e
     doInput name i
 where
  stop =
    do send ("-- " ++ name ++ " stops chatting --")
       quit

-- communication

send :: String -> GUI ()
send s = appendFile chatFile (s ++ "\n")

receiveChan :: GUI (CVar String)
receiveChan = readFileBlocking chatFile

-- output

doOutput :: Edit -> GUI ()
doOutput e =
  do cv <- receiveChan
     fork (read cv)
 where
  read cv =
    do s <- takeCVar cv
       putEnd e s
       (ysize,_) <- getSize e
       setYView e ysize
       updateTask
       read cv

-- input

doInput :: String -> Entry String -> GUI ()
doInput name i = cset i $ on return $
  do s <- getValue i
     setValue i ""
     send (name ++ ":" ++ s)

-- auxilary function

readFileBlocking :: FilePath -> GUI (CVar String)
readFileBlocking name =
  do v <- newCVar'
     fork (read v 0)
     result v
 where
  read v n =
    do h <- openFile name ReadMode
       hSeek h AbsoluteSeek n
       s <- hGetContents h
       if null s then delay 500
                 else putCVar v s
       read v (n + length s)

--------------------------------------------------
-- the end

