--------------------------------------------------------------------
-- "Fudgets in TkGofer"
-- Koen Claessen, 1996, adapted for new version, 1997.
--------------------------------------------------------------------

infixr 8 <@>, !@!

--------------------------------------------
-- The Fudget Monad type.

{-
type FM = Reader Window   -- reads window
        ( Writer [Frame]  -- collects frames
        ( GUI ))          -- acts as GUI
-}

type FM a = Window -> GUI ( a, [Frame] )
  in bindF, resultF
   , withWin, winObject
   , frameTrans, gui
   , runFM

-- instances

instance Monad FM where
  bind   = bindF
  result = resultF

bindF :: FM a -> (a -> FM b) -> FM b
m `bindF` k = \win ->
  do (a, frames1) <- m win
     (b, frames2) <- k a win
     result (b, frames1 ++ frames2)

resultF :: a -> FM a
resultF a = \win -> result (a, [])

--------------------------------------------
-- Functions acting on FM monad.

-- windows

withWin :: FM a -> String -> FM a
m `withWin` name = \_ -> m <@> window [title name]

-- frames

winObject :: Widget (WItem w) => (Window -> GUI (WItem w)) -> FM (WItem w)
winObject mf = \win ->
  do obj <- mf win
     fr  <- frame [] obj
     result (obj, [fr])

frameTrans :: ([Frame] -> [Frame]) -> FM a -> FM a
frameTrans f m = \win -> map (\(a, frs) -> (a, f frs)) (m win)

-- gui

gui :: GUI a -> FM a
gui m = \_ -> do a <- m; result (a, [])

runFM :: FM a -> GUI a
runFM fm =
  do (a, frames) <- fm (error "runFM: no window specified")
     mapl pack frames
     result a

-- a fixpoint

fixFM :: (GUI a -> FM a) -> FM a
fixFM mf =
  do v <- gui $ newGVar (error "fixFM: you're not lazy enough")
     a <- mf (readGVar v)
     gui $ writeGVar v a
     result a

--------------------------------------------
-- General Monadic extensions.

(<@>) :: Monad m =>   (a -> m b) -> m a -> m b
(!@!) :: Monad m => m (a -> m b) ->   a -> m b

f  <@> mx = do x <- mx; f x
mf !@! x  = do f <- mf; f x
  
--------------------------------------------
-- the end.

--------------------------------------------------------------------
-- Datatypes.

--------------------------------------------
-- Maybe

instance Text (Maybe a)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

stripMaybe :: Maybe a -> a
stripMaybe (Just a) = a

--------------------------------------------
-- Either.

instance Text (Either a b)

stripLeft :: Either a b -> Maybe a
stripLeft (Left a) = Just a
stripLeft _        = Nothing

stripRight :: Either a b -> Maybe b
stripRight (Right a) = Just a
stripRight _         = Nothing

stripEither :: Either a a -> a
stripEither (Left a)  = a
stripEither (Right a) = a

filterLeft :: [Either a b] -> [a]
filterLeft = map outl . filter isLeft

filterRight :: [Either a b] -> [b]
filterRight = map outr . filter isRight

isLeft :: Either a b -> Bool
isLeft = isNothing . stripRight

isRight :: Either a b -> Bool
isRight = isNothing . stripLeft

mapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
mapEither f g e = case e of
  Left a  -> Left (f a)
  Right b -> Right (g b)

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f g = stripEither . mapEither f g

infix 5 <+>
f <+> g = foldEither f g

swapEither :: Either a b -> Either b a
swapEither (Left a)  = Right a
swapEither (Right b) = Left b

splitEitherList :: [Either a b] -> ([a], [b])
splitEitherList xs = (filterLeft xs, filterRight xs)

outl :: Either a b -> a
outl (Left a) = a

outr :: Either a b -> b
outr (Right b) = b

--------------------------------------------
-- Input messages

data InputMsg a = InputChange a
                | InputDone String a

instance (Eq a) => Eq (InputMsg a) where
  InputChange a == InputChange b = a == b
  InputDone _ a == InputDone _ b = a == b
  _             == _             = False

instance Text (InputMsg a)

stripInputMsg :: InputMsg a -> a
stripInputMsg (InputChange a) = a
stripInputMsg (InputDone _ a) = a

tstInp :: (a -> b) -> InputMsg a -> b
tstInp f = f . stripInputMsg

mapInp :: (a -> b) -> InputMsg a -> InputMsg b
mapInp f (InputChange a) = InputChange (f a)
mapInp f (InputDone k a) = InputDone k (f a)

inputDone :: InputMsg a -> Maybe a
inputDone (InputDone _ a) = Just a
inputDone _               = Nothing

--------------------------------------------------------------------
-- The F constructor.

--------------------------------------------
-- Type F a b.

type Send a = a -> GUI ()

type F a b = Send b -> FM (Send a)
  in (>==<), (>*<), (>+<)
   , listF, idRightF, loopF, loopLeftF
   , shellF, fudgeTk
   , makeF, useF, makeF'

--------------------------------------------
-- Basic Combinators.

infixr 8 >+<, >*<
infixr 7 >==<

-- composition

(>==<) :: F b c -> F a b -> F a c
fudL >==< fudR
  = \sendC ->
  do sendB <- fudL sendC
     sendA <- fudR sendB
     result sendA

-- parallell composition

(>*<) :: F a b -> F a b -> F a b
fudL >*< fudR
  = \sendB ->
  do lSendA <- fudL sendB
     rSendA <- fudR sendB
     result (\a -> lSendA a `seq` rSendA a)

(>+<) :: F a b -> F c d -> F (Either a c) (Either b d)
fudL >+< fudR
  = \sendBD ->
  do sendA <- fudL (sendBD . Left)
     sendC <- fudR (sendBD . Right)
     result (sendA <+> sendC)

-- looping

loopF :: F a a -> F a a
loopF fud = \sendOut ->
  fixFM $ \mSendIn -> fud $ \a ->
    do sendIn <- mSendIn
       sendIn a
       sendOut a

loopLeftF :: F (Either a b) (Either a c) -> F b c
loopLeftF fud = \sendC -> map (. Right) $
  fixFM $ \mSendAB -> fud $ \ab ->
    do sendAB <- mSendAB
       (sendAB . Left <+> sendC) ab

-- list of Fudgets

listF :: Eq t => [(t, F a b)] -> F (t, a) (t, b)
listF fuds
  = \ftb ->
  do let taggedFudget (t, fud)
           = map (pair t) (fud (ftb . (pair t)))

     table <- mapl taggedFudget fuds
     
     result (\(t, a) -> (t `lookUpIn` table) a)
 where
  (a `lookUpIn`) = snd . head . filter ((a==).fst)
  pair t a = (t, a)

-- identity stream

idRightF :: F a b -> F (Either a c) (Either b c)
idRightF fud
  = \fbc ->
  do fa <- fud (fbc . Left)
     result ( fa <+> fbc . Right )

-- a Fudget in a window

shellF :: String -> F a b -> F a b
shellF name fud = \fb -> fud fb `withWin` name

--------------------------------------------
-- Starting Function.

-- equivalence of `fudlogue'

fudgeTk :: F a b -> IO ()
fudgeTk = start . void . runFM . ($ notConnected)

-- not connected

notConnected :: b -> GUI ()
notConnected _ = result ()

--------------------------------------------
-- Making and Using a Fudget.

makeF :: (Send b -> FM (Send a)) -> F a b
makeF = id

useF  :: F a b -> (Send b -> FM (Send a))
useF = id

makeF' :: (Send b -> GUI (Send a)) -> F a b
makeF' f = gui . f

--------------------------------------------------------------------
-- The Sp constructor.

--------------------------------------------
-- Stream Processors, type SP.

data SP a b = SP [b] (a -> SP a b)
-- in putSP, getSP, absF

--------------------------------------------
-- Basic constructors.

-- null

nullSP :: SP a b
nullSP = getSP (\_ -> nullSP)

-- recieve message

getSP :: (a -> SP a b) -> SP a b
getSP fsp = SP [] fsp

-- send messages

putSP :: [b] -> SP a b -> SP a b
putSP mesgs (SP bs fsp) = SP (mesgs ++ bs) fsp

--------------------------------------------
-- From SP to F.

absF :: SP a b -> F a b
absF sp = makeF' $ \send ->
  do vSP <- newGVar (\_ -> sp)

     let sendA a =
           do fsp <- readGVar vSP
              let (SP mesgs fsp') = fsp a
              writeGVar vSP fsp'
              seqs [ send m | m <- mesgs ]
  
     sendA undefined
     result sendA

--------------------------------------------------------------------
-- Primitive Fudgets.

-- null

nullF :: F a b
nullF = makeF $ \_ -> result notConnected

nullLF :: F a b
nullLF = labelF ""

-- label

labelF :: String -> F a b
labelF s = makeF
  $ \_ ->
  do winObject $ label [text s]
     result notConnected

-- buttons

data Click = Click

buttonF :: String -> F Click Click
buttonF s = makeF
  $ \send ->
  do winObject $ button [text s, command (send Click)]
     result (\_ -> send Click)

toggleButtonF :: String -> F Bool Bool
toggleButtonF s = makeF
  $ \send ->
  do winObject $ checkbutton
       [ text s
       , self $ \but -> command (send <@> getValue but)
       ]
     result send

-- display

displF :: (Widget (Entry d), HasInput WItem Entry0 d, HasBorder (Entry d))
       => F d a
displF = makeF
  $ \_ ->
  do ent <- winObject $ entry [relief "sunken"]
     result (setValue ent)

intDispF :: F Int a
intDispF = displF

displayF :: F String a
displayF = displF

-- input

key' :: TkEvent
key' = Tk_Event "<KeyPress>"

inputF :: (Widget (Entry i), HasInput WItem Entry0 i, Eq i)
       => F i (InputMsg i)
inputF = makeF
  $ \send ->
  do ent <- winObject $ entry
       [ self $ \ent -> on return $ send.InputDone "Return" <@> getValue ent
       , self $ \ent -> on key'   $ send.InputChange        <@> getValue ent
       ]
     result $ \s ->
         (map (/= s) $ getValue ent)
           ==> do { setValue ent s; updateTask; send (InputDone "Return" s)}

stringF :: F String (InputMsg String)
stringF = inputF

intF :: F Int (InputMsg Int)
intF = inputF

-- quitting

quitF :: F a b
quitF = makeF $ \_ -> result (\_ -> quit)

quitButtonF :: F a b
quitButtonF = quitF >==< buttonF "Quit" >==< nullF

-- timer

data Tick = Tick

timerF :: F (Maybe (Int, Int)) Tick
timerF = makeF'
  $ \send ->
  do clk <- timer [command (send Tick), active False]
     result $ \a -> case a of
       Nothing -> cset clk (active False)
       Just (i,j) -> do { cset clk (initValue i); cset clk (active True) }

-- invisible (in fact: SP's)

mapF :: (a -> b) -> F a b
mapF f = makeF $ \send -> result (send . f)

-- debugging

primitive primTrace "primTrace" :: String -> a -> a

traceMsg s a b = primTrace ("{{" ++ s ++ ":" ++ show' a ++ "}}") b

spyF :: String -> F a b -> F a b
spyF mesg fud = makeF $ \sendB ->
 do f <- useF fud (treat "out" sendB)
    result (treat "in" f)
 where
  treat t f = \a -> traceMsg (mesg ++ "/" ++ t) a (f a)

--------------------------------------------------------------------
-- Abstractions and Generalisations.

--------------------------------------------
-- Class Fudget.

class Fudget f where
  fudget :: f a b -> F a b

instance Fudget F where 
  fudget = id
            
instance Fudget SP where
  fudget = absF

instance Fudget (->) where
  fudget = mapF

--------------------------------------------
-- General Combinators.

infixr 8 >||<
infixr 7 >=<

-- general composition

(>=<) :: (Fudget f, Fudget g) => f b c -> g a b -> F a c
f >=< g = fudget f >==< fudget g

-- general parallel composition

(>||<) :: (Fudget f, Fudget g) => f a b -> g c d -> F (Either a c) (Either b d)
f >||< g = fudget f >+< fudget g

-- ugly combinators

infixr 7 >=^<, >=^^<, >^=<, >^^=<

(>=^<)  :: F a b    -> (c -> a) -> F c b
(>=^^<) :: F a b    -> SP c a   -> F c b
(>^=<)  :: (a -> b) -> F c a    -> F c b
(>^^=<) :: SP a b   -> F c a    -> F c b

(>=^<)  = (>=<)
(>=^^<) = (>=<)
(>^=<)  = (>=<)
(>^^=<) = (>=<)

--------------------------------------------
-- Strip.

class Strip d a where
  strip :: d -> a

instance Strip (Maybe a) a where
  strip = stripMaybe

instance Strip (Either a a) a where
  strip = stripEither

instance Strip (InputMsg a) a where
  strip = stripInputMsg

--------------------------------------------
-- Maybe SP's.

maybeSP :: (a -> Maybe b) -> SP a b
maybeSP f = getSP $ \a -> case f a of
  Nothing -> maybeSP f
  Just b  -> putSP [b] $ maybeSP f

--------------------------------------------------------------------
-- Derived Combinators.

--------------------------------------------
-- F combinators.

loopRightF :: F (Either a b) (Either c b) -> F a c
loopRightF f = loopLeftF $ absF (mapSP swapEither)
                                >==< f >==<
                           absF (mapSP swapEither)

startupF :: [a] -> F a b -> F a b
startupF as fud = fud >==< absF (putSP as idSP)

idLeftF :: F a b -> F (Either c a) (Either c b)
idLeftF f = absF (mapSP swapEither)
              >==< idRightF f >==<
            absF (mapSP swapEither)

--------------------------------------------
-- SP combinators.

idSP :: SP a a
idSP = getSP $ \a -> putSP [a] idSP

stateSP :: (a -> b -> b) -> b -> SP a b
stateSP f b
  = getSP $ \a -> let b' = f a b in putSP [b'] $ stateSP f b'

mapSP :: (a -> b) -> SP a b
mapSP f
  = stateSP (\a _ -> f a) undefined

filterSP :: (a -> Bool) -> SP a a
filterSP f
  = getSP $ \a -> (if f a then putSP [a] else id) $  filterSP f

mapstateSP :: (a -> b -> (a, [c])) -> a -> SP b c
mapstateSP f a
  = getSP $ \b -> let (a',cs) = f a b in putSP cs $ mapstateSP f a'

zipSP :: [a] -> SP b (a, b)
zipSP []     = nullSP
zipSP (x:xs) = getSP $ \b -> putSP [(x, b)] $ zipSP xs

splitSP :: SP (a, b) (Either a b)
splitSP = getSP $ \(a, b) -> putSP [Left a, Right b] $ splitSP

dupSP :: SP a (Either a a)
dupSP =getSP $ \a -> putSP [Left a, Right a] $ dupSP

concatSP :: SP [a] a
concatSP = getSP $ \xs -> putSP xs $ concatSP

-- input

inputDoneSP :: SP (InputMsg a) a
inputDoneSP = getSP $ \i -> case i of
  InputDone _ s -> putSP [s] $ inputDoneSP
  _             -> inputDoneSP

--------------------------------------------------------------------
-- Placer Fudgets.

--------------------------------------------
-- Placer type.

type Placer = [Frame] -> [Frame]

-- the placer fudget

placerF :: Placer -> F a b -> F a b
placerF pl fud = makeF $ \fb -> frameTrans pl (useF fud fb)

--------------------------------------------
-- Several Placers.

horizontalP :: Placer
horizontalP frs = [horizontal frs]

verticalP :: Placer
verticalP frs = [vertical frs]

matrixP :: Int -> Placer
-- matrixP n frs = [matrix n frs] 
matrixP n frs = [ foldl1 (<|<) . map (foldl1 (^-^))
                . transpose . group n
                $ frs
                ]

group :: Int -> [a] -> [[a]]
group n = map (take n) . takeWhile (not.null) . iterate (drop n)

--------------------------------------------
-- Placer Combinators.

revP :: Placer -> Placer
revP pl frs = pl (reverse frs)

numbP :: Int -> Placer -> Placer -> Placer
numbP n pl1 pl2 frs = pl1 (take n frs) ++ pl2 (drop n frs)

--------------------------------------------
-- Derived Fudgets.

labLeftOfF :: String -> F a b -> F a b
labLeftOfF lab fud = placerF horizontalP $ labelF lab >*< fud

--------------------------------------------------------------------
-- the end.

