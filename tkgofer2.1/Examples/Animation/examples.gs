------------------------------------------
-- Animation Examples
------------------------------------------

-- need animation.gs

------------------------------------------
-- Examples

dude  = bitmapIm "dude.xbm"
shark = bitmapIm "shark.xbm"

run :: Animation -> IO ()
run = start . animate

main = run exam1

-- example 1

tracker :: Behavior Time -> [Behavior Image] -> Behavior Image
tracker dt
  = foldr1 over
  . zipWith (\n -> later (consT n * dt)
                 . moveXY mouseX mouseY
            ) [0.0..]

exam1 :: Animation
exam1 = tracker (consT 2.0)
  [ textIm s
  | s <- words "Time flows like a river"
  ]

-- example 2

around :: Behavior Image -> Behavior Image -> Behavior Image
im1 `around` im2
      = moveXY (-wiggle) (-wiggle') im1
 `over` moveXY wiggle  wiggle'   im2

exam2 :: Animation
exam2 = shark `around` dude

-- example 3

counter :: Int -> Behavior Int
counter n = consT n `untilB` lbp *=> counter (n-1)
                         .|. rbp *=> counter (n+1)

exam3 :: Animation
exam3 = behIm (counter 0)

-- example 4

cycleIt :: [Behavior a] -> Behavior a
cycleIt (x:xs) = x `untilB` lbp *=> cycleIt (xs ++ [x])

exam4 :: Animation
exam4 = tracker (consT 3.0)
      $ copy 5
      $ cycleIt [ textIm s | s <- words "One Two Three Four Five" ]

-- example 5

exam5 :: Animation
exam5 = moveXY mouseX mouseY (behIm time)

------------------------------------------
-- the end.
