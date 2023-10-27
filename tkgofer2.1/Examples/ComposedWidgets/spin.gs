-- Spinner ---------------------------------------------
-- combination of a SpinButton and an entryfield

data Spinner0 v = Spinner (Entry v, SpinButton, Label)
type Spinner = WItem (Spinner0 Int)

spinner :: [Conf Spinner] -> Window -> GUI Spinner
spinner cs w =
  do e  <- entry [] w
     l  <- label [padx 4, pady 0, anchor "e"] w
     s  <- spinButton
               [ spinUp (updValue (+1) e)
               , spinDn (updValue (+(-1)) e)
               ] w
     composeWidget (Spinner (e,s,l)) ((e ^-^ l) << s ) cs

spinnerE :: Spinner -> Entry Int 
spinnerE i = let (Spinner (e,_,_)) = getWidget i in e

spinnerS :: Spinner -> SpinButton
spinnerS i = let (Spinner (_,s,_)) = getWidget i in s

spinnerL :: Spinner -> Label
spinnerL i = let (Spinner (_,_,l)) = getWidget i in l

instance Widget (WItem (Spinner0 Int)) where
  cset w c = let s = spinnerS w
                 e = spinnerE w
                 l = spinnerL w
                 newc = c w
             in case newc of
                 Tk_Command cmd ->
                    do cset s $ spinUp $ do updValue (+1) w; cmd
                       cset s $ spinDn $ do updValue (+(-1)) w; cmd
                       cset e  $ on return cmd
                 Tk_Text x  -> cset l $ (const newc)
                 Tk_Width x -> cset e $ (const newc)
                 otherwise ->
                    do cset s (const newc)
                       cset e (const newc)
                       cset l (const newc)
 
  onArgs e r a w = onArgs e r a (spinnerE w)
 
 
instance HasCommand Spinner 
instance HasBackground Spinner 
instance HasForeground Spinner
instance HasBorder Spinner 
instance HasText  Spinner
instance HasWidth Spinner
 
instance HasInput WItem Spinner0 Int where
  getValue = getValue . spinnerE
  setValue = setValue . spinnerE

-- SpinButton ------------------------------------------
-- combination of two Buttons

data SpinButton0 = SpinButton (Button,Button)
type SpinButton  = WItem SpinButton0
 
getSpinUp :: SpinButton -> Button
getSpinUp w = let SpinButton (u,d) = getWidget w in u
 
getSpinDn :: SpinButton -> Button
getSpinDn w = let SpinButton (u,d) = getWidget w in d
 
spinUp,spinDn :: GUI () -> Conf SpinButton
spinUp c = myConf (\w -> cset (getSpinUp w) (command c))
spinDn c = myConf (\w -> cset (getSpinDn w) (command c))
 
instance HasBackground SpinButton
instance HasForeground SpinButton
instance Widget SpinButton where
  cset w c =
    do cset (getSpinUp w) (const (c w))
       cset (getSpinDn w) (const (c w))

  cget w f = result tk_defaultValue
  onArgs _ _ _ _ = Tk_DummyConf
 
spinButton :: [Conf SpinButton] -> Window -> GUI SpinButton
spinButton cs w = do
  b1 <- button [bitmap "up.bmp", pady 0, padx 0] w
  b2 <- button [bitmap "dn.bmp", pady 0, padx 0] w
  composeWidget (SpinButton (b1,b2)) (b1 ^^ b2) cs
