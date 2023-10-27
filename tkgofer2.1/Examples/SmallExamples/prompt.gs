
 
data Prompt0 v = Prompt (Entry v, Label)
type Prompt  v = WItem (Prompt0 v)

promptE :: Prompt v -> Entry v
promptE p = let Prompt (e,l) = getWidget p in e

promptL :: Prompt v -> Label
promptL p = let Prompt (e,l) = getWidget p in l


prompt :: (Widget (Prompt a), GUIValue a) 
       => [Conf (Prompt a)] -> Window -> GUI (Prompt a)
prompt cs w =
  do l <- label [] w
     e <- entry [] w
     composeWidget (Prompt (e,l)) (l << e) cs


instance Widget (Entry v) => Widget (Prompt v) where
  cset w c =
    case (c w) of
      (Tk_Text s)      -> cset (promptL w) (const (Tk_Text s))
      (Tk_InitValue x) -> cset (promptE w) (const (Tk_InitValue x))
      otherwise        -> do cset (promptE w) (const (c w))
                             cset (promptL w) (const (c w))
  onArgs e s a = onArgs e s a . promptE


instance HasText (Prompt v)
instance HasForeground (Prompt v)
instance HasBackground (Prompt v)
instance HasInput WItem Entry0 v => HasInput WItem Prompt0 v where
  getValue = getValue . promptE
  setValue = setValue . promptE

 
ex_prompt :: IO ()
ex_prompt = start $
  do w <- window [ title "Simple Adder 2" ]
     i <- prompt [ text "Press the return key"
                 , initValue 0, self $ on return . updValue (+1) ] w
     pack i

