-- primitive file IO
-- based on Tk primitives

-- directories 

cdDir  :: String -> GUI ()
cdDir s = tk_putTcl ["cd", tk_secure s]

getDir :: String -> GUI [String]
getDir mask = 
  do{x <- tk_getTcl [ "dirlist" , tk_secure mask]
    ;result (lines x)
    }

pwd  :: GUI String
pwd = tk_getTcl ["pwd"]

-- file checks

fileExists  :: String -> GUI Bool
fileExists s =
 do {x <- tk_getTcl ["file exists", tk_secure s]
    ;tk_fromGUI x
    }

fileIsDir   :: String -> GUI Bool
fileIsDir s =
 do {x <- tk_getTcl ["file isdirectory", tk_secure s]
    ;tk_fromGUI x
    }

fileIsFile   :: String -> GUI Bool
fileIsFile s =
 do {x <- tk_getTcl ["file isfile", tk_secure s]
    ;tk_fromGUI x
    }


-- files 

data FileId = FileId String

closeFile :: FileId -> GUI ()
closeFile (FileId s) = tk_putTcl ["close", s]

isEof     :: FileId -> GUI Bool
isEof (FileId s) = 
  do { x <- tk_getTcl ["eof", s]
     ; tk_fromGUI x
     }

flush     :: FileId -> GUI ()
flush (FileId s) = tk_putTcl ["flush", s]

getsFile  :: FileId -> GUI String
getsFile (FileId s) = tk_getTcl ["gets", s]

openFile  :: String -> String -> GUI FileId
openFile s t = 
  do {x <- tk_getTcl ["open", s, t]
     ;result (FileId x)
     }
