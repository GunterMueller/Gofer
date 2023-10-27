--
-- yesno widget with two different command options
-- Ton Vullinghs 
-- November '96
--

data YesNo0 = YesNo (Button, Button, Message)  
type YesNo  = WItem YesNo0

yesno :: [Conf YesNo] -> Window -> GUI YesNo
yesno cs w = do
  m  <- message [borderWidth 30] w
  b0 <- button [text "Yes", width 6,padx 10, pady 10] w
  b1 <- button [text "No", width 6, padx 10, pady 10] w
  composeWidget (YesNo (b0,b1,m)) (m ^-^ (b0 <*+< b1)) cs

instance Widget YesNo where
  cset w c = void $ tk_showConf "" (c w)
  cget w f = result tk_defaultValue
  onArgs _ _ _ _ = Tk_DummyConf

instance HasBackground YesNo
instance HasForeground YesNo

ynYes :: GUI () -> Conf YesNo
ynYes a = myConf (\(WItem (YesNo (w,_,_)) _) -> cset w (command a))

ynNo  :: GUI () -> Conf YesNo
ynNo  a = myConf (\(WItem (YesNo (_,w,_)) _) -> cset w (command a))

ynMsg :: String -> Conf YesNo
ynMsg t = myConf (\(WItem (YesNo (_,_,w)) _) -> cset w (text t))


-- Example -----------------------------------------


main :: IO ()
main =
  start $  do
    w <- window []
    e <- entry [self (on return . updValue (+1)) ] w
    b <- button [text "Quit", command maybeQuit  ] w
    pack (e << b)

maybeQuit :: GUI ()
maybeQuit = openWindow [title "Warning"] $ \w ->
  yesno [ ynMsg "Really Quit"
        , ynYes quit
        , ynNo  (closeWindow w)
        ] w
