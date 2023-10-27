------------------------------------------
-- Animation Library
-- Koen Claessen, Thomas Nordin, 1996
-- new version, Koen Claessen, July 1997
------------------------------------------

infixl 3 `untilB`
infixl 4 .|.
infixl 5 \=>, *=>

infixl `over`

type Animation = Behavior Image

------------------------------------------
-- Basic Types

type Coor = (Int, Int)
type Time = Float

-- Primitive Events

data PrEvent
  = LeftButton
  | RightButton
  | Mouse Coor
  | Nil

instance Eq PrEvent where
  LeftButton  == LeftButton  = True
  RightButton == RightButton = True
  Mouse _     == Mouse _     = True
  Nil         == Nil         = True
  _           == _           = False

------------------------------------------
-- Behavior

data Behavior a = Bh (Time -> [PrEvent] -> (Behavior a, a))

instance Functor Behavior where
  map = lift1

consT :: a -> Behavior a
consT = lift0

instance Eq a => Eq (Behavior a)
instance Text (Behavior a)

instance Num a => Num (Behavior a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  (-) = lift2 (-)
  (/) = lift2 (/)
  negate x = fromInteger 0 - x
  fromInteger = consT . fromInteger

lift0 :: a -> Behavior a
lift1 :: (a -> b) -> Behavior a -> Behavior b
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c
                                                              -> Behavior d

lift0 a =
  Bh $ \t ev ->     (lift0 a, a)

lift1 f = \(Bh ba) ->
  Bh $ \t ev -> let (ba',a) = ba t ev
                 in (lift1 f ba',f a)

lift2 f = \(Bh ba) (Bh bb) ->
  Bh $ \t ev -> let (ba',a) = ba t ev
                    (bb',b) = bb t ev
                 in (lift2 f ba' bb',f a b)

lift3 f = \(Bh ba) (Bh bb) (Bh bc) ->
  Bh $ \t ev -> let (ba',a) = ba t ev
                    (bb',b) = bb t ev
                    (bc',c) = bc t ev
                 in (lift3 f ba' bb' bc',f a b c)

runBh :: Behavior a -> Time -> [PrEvent] -> (Behavior a, a)
runBh (Bh bf) = bf

------------------------------------------
-- Events

type Event a = Behavior (Maybe (Time,a))
  in untilB, (\=>), (.|.), pred, primEvent, never

instance Functor Event where
  map f ev = ev \=> f

instance Monad Event

instance Monad0 Event where
  zero = never

instance MonadPlus Event where
  (++) = (.|.)

-- until

untilB :: Behavior a -> Event (Behavior a) -> Behavior a
Bh ba `untilB` Bh be
  = Bh $ \t ev -> case be t ev of
     (be', Nothing)        -> let (ba',a) = ba t ev in (ba' `untilB` be',a)
     (_  , Just (t,Bh ba)) -> ba t []

(.|.) :: Event a -> Event a -> Event a
(.|.) = lift2 (++)

-- combinators

(\=>) :: Event a -> (a -> b) -> Event b
ev \=> f = map (map (\(p,a) -> (p,f a))) ev

(*=>) :: Event a -> b -> Event b
ev *=> b = ev \=> \_ -> b

stream :: Event a -> a -> Behavior a
stream ev = f where f a = consT a `untilB` ev \=> f

-- predicates

pred :: Behavior Bool -> Event ()
pred (Bh bp)
  = Bh $ \t ev -> let (bp',_) = bp t ev 
                   in ( pred bp', map (\t -> (t,()))
                                   (find (\t -> snd (bp t ev)) t (nextInt t)))
 where
  find f t0 t1 = case (filter f) (fromTo t0 t1) of
    (t:_) -> Just t
    _     -> Nothing

  nextInt = fromInteger . truncate . (+1.0)

  fromTo t0 t1
    = map ((/10.0).fromInteger) [truncate (10.0 * t0) .. truncate (10.0 * t1)]

never :: Event a
never = Bh $ \t ev -> (never, Nothing)

-- primitive events

primEvent :: PrEvent -> (PrEvent -> a) -> Event a
primEvent patEv f = this
 where
  this = Bh $ \t evs -> case filter (patEv==) evs of
    (e:_) -> (error "Just had no cont", Just (t, f e))
    _     -> (this, Nothing)

-- predefined events

lbp     = primEvent LeftButton        (\_           -> ())
rbp     = primEvent RightButton       (\_           -> ())
mouse   = primEvent (Mouse undefined) (\(Mouse pos) -> pos)

------------------------------------------
-- Standard Behaviors

time :: Behavior Time
time = Bh $ \t e -> (time,t)

timeTrans :: Behavior a -> Behavior Time -> Behavior a
timeTrans = trans []
 where
  trans store (Bh ba) (Bh bt)
    = Bh $ \t evs ->
       let (bt',t')     = bt t evs
           (ba',a )     = ba t' now
           store'       = store ++ [(t,evs)]
           (now,later)  = splitStore t' store'
        in (trans later ba' bt', a)

  splitStore t ((t',es):st)
    | t >= t'     = let (es',st') = splitStore t st in (es++es',st')
  splitStore t st = ([],st)
  
slower, later :: Behavior Time -> Behavior a -> Behavior a
slower dt = (`timeTrans` (time / dt))
later  dt = (`timeTrans` (time - dt))

freeze :: Behavior a -> Behavior a
freeze (Bh ba) = Bh $ \t ev -> let (_,a) = ba t ev in (consT a,a)

mouseXY :: Behavior (Int, Int)
mouseXY = stream mouse (0,0)
mouseX  = map fst mouseXY
mouseY  = map snd mouseXY

atRate :: Num (Behavior a) => Behavior a -> Behavior a
atRate ba = rate (fromInteger 0) ba (fromInteger 0)
 where
  rate n ~(Bh dx) x
   = let x' = x + n
      in Bh $ \t ev -> let (dx',d) = dx t ev
                        in (rate d dx' x', x')

wiggle, wiggle' :: Behavior Int
wiggle  = map (truncate . (100.0 *) . sin . (*0.2)) time
wiggle' = map (truncate . (100.0 *) . cos . (*0.2)) time

------------------------------------------
-- Images

type Image = (Coor -> Coor) -> Canvas -> GUI ()

drawIm :: Image -> Canvas -> GUI ()
drawIm im c =
  do clearCanvas c
     im id c
     updateTask

guiIm :: (Coor -> [Conf c] -> Canvas -> GUI c) -> [Conf c] -> Image
guiIm obj cs
  = \f c -> void $ obj ((\(x,y) -> (x+maxX/2,y+maxY/2)) (f (0,0))) cs c

textIm :: String -> Behavior Image
textIm s = consT $ guiIm ctext [ text s ]

bitmapIm :: String -> Behavior Image
bitmapIm s = consT $ guiIm cbitmap [ bitmap s ]

behIm :: Text a => Behavior a -> Behavior Image
behIm = map (guiIm ctext . (:[]) . text . show)

timeIm :: Behavior Image
timeIm = behIm time

over :: Behavior Image -> Behavior Image -> Behavior Image
over = lift2 over'
 where
  im1 `over'` im2 = \f c -> do { im1 f c; im2 f c }

moveXY :: Behavior Int -> Behavior Int -> Behavior Image -> Behavior Image
moveXY = lift3 move'
 where
  move' dx dy im = \f c -> im ( f . \(x,y) -> (x+dx,y+dy) ) c

------------------------------------------
-- Animate!

maxX = 400
maxY = 400

mMouse :: TkEvent
mMouse = Tk_Event "<Motion>"

mousePos :: Coor -> PrEvent
mousePos (x,y) = Mouse (x - maxX/2, y - maxY/2)

animate :: Behavior Image -> GUI ()
animate anim =
  do win <- window [ title "Animation" ]
     but <- button [ text "Quit", command quit ] win
     stE <- newMVar []
     can <- canvas [ width maxX
                   , height maxY
                   , on (click 1) $        updateV (LeftButton :)  stE
                   , on (click 2) $        updateV (RightButton :) stE
                   , onxy mMouse  $ \xy -> updateV (mousePos xy :) stE
                   ] win

     pack (can ^^ but)

     t0 <- getTicks
     let loop beh =
           do evs <- readV stE
              stE =: []
              t1 <- getTicks
              let t = fromInteger (t1 - t0) / 100.0
              let (beh', img) = runBh beh t evs
              drawIm img can
              loop beh'
      in fork $ loop anim 

------------------------------------------
-- the end.
