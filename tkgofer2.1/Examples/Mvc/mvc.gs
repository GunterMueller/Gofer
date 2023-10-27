-------------------------------------------------
-- MVC - Model View Controller

type Send a = a -> GUI ()

-------------------------------------------------
-- Controller Widget

data Control0 v = Control (Clipboard v) (Clipboard [Send v])
type Control v  = TItem (Control0 v)

instance Widget (Control v) where
  cset w c = void $ tk_showConf "" (c w)

instance ( HasInput TItem Clipboard0 v 
         , HasInput TItem Clipboard0 [Send v]
         ) => HasInput TItem Control0 v where   
  getValue (TItem (Control vVal _   ) _)     = getValue vVal
  setValue (TItem (Control vVal vTab) _) val =
    do setValue vVal val
       sends <- getValue vTab
       seqs [ send val | send <- sends ]

  readOnly _ _ = Tk_DummyConf

control :: ( Widget (Clipboard a)
           , HasInput TItem Clipboard0 [Send a]
           , Widget (TItem (Control0 a))
           ) => [Conf (Control a)] -> GUI (Control a)
control cs =
  do vVal <- clipboard []
     vTab <- clipboard [initValue []]
     composeAbsWidget (Control vVal vTab) cs
  

composeAbsWidget v cs = do
  let t = TItem v ""
  csets t cs
  result t

-------------------------------------------------
-- View class

class View w v where
  updateView :: w -> Send v
  invokeView :: w -> Send v -> GUI ()

-------------------------------------------------
-- View control

addView w c =
  do val <- getValue c
     updateView w val
     invokeView w (setValue c)
     addMethod c (updateView w)

addMethod (TItem (Control vVal vTab) _) send = 
  do updValue (send :) vTab

mvc :: (Widget w, View w v, HasInput TItem Control0 v) => Control v -> Conf w
mvc c = myConf $ \w -> addView w c

-------------------------------------------------
-- the end.

