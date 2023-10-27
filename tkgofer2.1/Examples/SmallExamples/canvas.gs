
ex_canvas :: IO ()
ex_canvas = start $ do
  do w <- window [title "Move It!"]
     c <- canvas [background "white", width 200, height 200] w
     r <- crect (10,10) (50,50) opts c
     l <- cline (70,70) (120,120) opts c
     o <- coval (150,150) (200,200) opts c
     t <- ctext (10,130) [ text "hello world", moveIt, raiseIt] c
     pack c

opts :: HasFillColor a => [Conf a]
opts = [penWidth 3, penColor "red", fillColor "yellow", moveIt, raiseIt]

moveIt :: HasCoords a => Conf a
moveIt = self (onxy (motion 1) . moveIt') where
  moveIt' w (x,y) =
     do ((x',y'):ys) <- getCoords w
        moveObject w (x - x', y -y')

raiseIt :: HasCoords a => Conf a
raiseIt = self (on (click 1) . raiseObject)
