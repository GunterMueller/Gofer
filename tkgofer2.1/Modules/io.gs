----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- io.gs

-- need prelude.gs
-- need guiMonad.gs

---------------------------------------------------------------------------
-- Some datatypes

type FilePath = String
type TkFileId = String

data Handle
  = TkFile TkFileId FilePath
  | TkChan TkFileId

stdin, stdout, stderr :: Handle
stdin  = TkChan "stdin"
stdout = TkChan "stdout"
stderr = TkChan "stderr"

instance Eq Handle where
  TkFile s _ == TkFile t _ = s == t
  TkChan s   == TkChan t   = s == t
  _          == _          = False

tk_getFileId :: Handle -> TkFileId
tk_getFileId (TkFile s _) = s
tk_getFileId (TkChan s)   = s

data IOMode
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode

data SeekMode 
  = AbsoluteSeek
  | RelativeSeek
  | SeekFromEnd

---------------------------------------------------------------------------
-- File IO functions

openFile :: FilePath -> IOMode -> GUI Handle
openFile file mode =
  do curr <- filePwd
     id   <- tk_getTcl ["open", tk_secure file, mode']
     result (TkFile id (file' curr))
 where 
   mode' = case mode of
     ReadMode      -> "r"
     WriteMode     -> "w"
     AppendMode    -> "a"
     ReadWriteMode -> "w+"
   
   file' curr
     | take 1 file == "/" = file
     | otherwise          = curr ++ "/" ++ file
   
hClose :: Handle -> GUI ()
hClose handle = tk_putTcl ["close", tk_getFileId handle]

hGetLine :: Handle -> GUI String
hGetLine handle = tk_getTcl ["gets", tk_getFileId handle]

hGetChar :: Handle -> GUI Char
hGetChar handle = map head (hGetString handle 1)

hGetString :: Handle -> Int -> GUI String
hGetString handle n = tk_getTcl ["read", tk_getFileId handle, show n]

hGetContents :: Handle -> GUI String
hGetContents = tk_lazyRead

hPutChar :: Handle -> Char -> GUI ()
hPutChar handle c = hPutStr handle [c]

hPutStr :: Handle -> String -> GUI ()
hPutStr handle s = tk_output s $ \s' ->
  tk_putTcl ["puts", "-nonewline", tk_getFileId handle, s']

hPutStrLn :: Handle -> String -> GUI ()
hPutStrLn handle s = tk_output s $ \s' ->
  tk_putTcl ["puts", tk_getFileId handle, s']

hPrint :: Text a => Handle -> a -> GUI ()
hPrint h x = hPutStrLn h (show x)

hFileSize :: Handle -> GUI Int
hFileSize = tk_handleGet "size"

hIsEOF :: Handle -> GUI Bool
hIsEOF handle = do x <- tk_getTcl ["eof", tk_getFileId handle]; tk_fromGUI x

isEOF :: GUI Bool
isEOF = hIsEOF stdin

hFlush :: Handle -> GUI ()
hFlush handle = tk_putTcl ["flush", tk_getFileId handle]

hSeek :: Handle -> SeekMode -> Int -> GUI ()
hSeek handle mode i = tk_putTcl ["seek", tk_getFileId handle, show i, mode']
 where
  mode' = case mode of
    AbsoluteSeek -> "start"
    RelativeSeek -> "current"
    SeekFromEnd  -> "end"

hIsReadable :: Handle -> GUI Bool
hIsReadable (TkChan "stdin") = result True
hIsReadable (TkChan _)       = result False
hIsReadable (TkFile _ name)  = tk_fileGet "readable" name

hIsWritable :: Handle -> GUI Bool
hIsWritable (TkChan "stdin") = result False
hIsWritable (TkChan _)       = result True
hIsWritable (TkFile _ name)  = tk_fileGet "writable" name

hIsExecutable :: Handle -> GUI Bool
hIsExecutable (TkChan _)      = result False
hIsExecutable (TkFile _ name) = tk_fileGet "executable" name

fileCd :: FilePath -> GUI ()
fileCd s = tk_putTcl ["cd", tk_secure s]

filePwd :: GUI FilePath
filePwd = tk_getTcl ["pwd"]

fileDir :: FilePath -> GUI [FilePath]
fileDir pat = do x <- tk_getTcl ["glob -nocomplain", pat']
                 result (words x)
 where
  pat' | pat == "" = "*"
       | otherwise = pat

fileIsDir :: FilePath -> GUI Bool
fileIsDir = tk_fileGet "isdirectory"

fileIsFile :: FilePath -> GUI Bool
fileIsFile = tk_fileGet "isfile" 

fileExists :: FilePath -> GUI Bool
fileExists = tk_fileGet "exists"

---------------------------------------------------------------------------
-- Auxilary functions

tk_fileGet :: GUIValue a => String -> FilePath -> GUI a
tk_fileGet what name =
  do x <- tk_getTcl ["file", what, tk_secure name]
     tk_fromGUI x

tk_handleGet :: GUIValue a => String -> Handle -> GUI a
tk_handleGet what (TkChan n)      = error ("Can't ask " ++ n ++ " for " ++ what)
tk_handleGet what (TkFile _ name) = tk_fileGet what name

---------------------------------------------------------------------------
-- Easy functions for reading and writing files

withFile :: FilePath -> IOMode -> (Handle -> GUI a) -> GUI a
withFile name mode f =
  do h <- openFile name mode
     a <- f h
     hClose h
     result a

infoFile :: FilePath -> (Handle -> GUI a) -> GUI a
infoFile name f = withFile name ReadMode f

-- writing

writeFileLines :: FilePath -> [String] -> GUI ()
writeFileLines name xs
  = withFile name WriteMode (void . (`mapl` xs) . hPutStrLn)

writeFile :: FilePath -> String -> GUI ()
writeFile name s
  = withFile name WriteMode (`hPutStr` s)

appendFile :: FilePath -> String -> GUI ()
appendFile name s
  = withFile name AppendMode (`hPutStr` s)

-- reading (dirty implementation)

readFile' :: FilePath -> GUI String
readFile' name =
  do s <- readFile name -- (is lazy, so force evaluation)
     eval s
     result s
 where
  eval (c:s) = eval s
  eval []    = result ()

readFileLines' :: FilePath -> GUI [String]
readFileLines' = map lines . readFile'

-- lazy

readFile :: FilePath -> GUI String
readFile name =
  do b <- fileExists name
     if b then result (openfile name)
          else result ""

readFileLines :: FilePath -> GUI [String]
readFileLines = map lines . readFile

-- auxilary function for lazy handle reading

tk_lazyRead :: Handle -> GUI String
tk_lazyRead h = interleaveGUI $
  do b <- hIsEOF h
     if b then do hClose h
                  result ""
          else do r  <- hGetString h blockSize
                  rs <- tk_lazyRead h
                  result (r++rs)
 where
  blockSize = 30

---------------------------------------------------------------------------
-- Execute a shell command

system :: String -> GUI String
system comm = tk_getTcl ["exec", comm]

---------------------------------------------------------------------------
-- Dialogues

fileOpenDialogue :: GUI (Maybe FilePath)
fileOpenDialogue =
  do x <- tk_getTcl ["tk_getOpenFile"]
     case x of
       ""   -> result Nothing
       name -> result (Just name)

fileSaveDialogue :: GUI (Maybe FilePath)
fileSaveDialogue =
  do x <- tk_getTcl ["tk_getSaveFile"]
     case x of
       ""   -> result Nothing
       name -> result (Just name)

---------------------------------------------------------------------------
-- the end.

