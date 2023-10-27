--------------------------------------------
-- "Fudgets in TkGofer"
-- fudExamples.gs : Examples.
-- Koen Claessen, 1996
--------------------------------------------

fudlogue = fudgeTk

--------------------------------------------
-- "Your first 8 Fudgets programs",
-- from the html page:
-- http://www.cs.chalmers.se/ComputingScience/Research/ ...
--      ... Functional/Fudgets/Intro/
--------------------------------------------

--------------------------------------------
-- 1. The "Hello, world!" program 

main1 = fudlogue (shellF "Hello" (labelF "Hello, world!"))


--------------------------------------------
-- 2. The "Hello, world!" program with a Quit button 

main2 = fudlogue (shellF "Hello" (labelF "Hello, world!" >+< quitButtonF))


--------------------------------------------
-- 3. A program to test the factorial function
-- (version 1) 

main3 = fudlogue (shellF "Fac" facF)

facF = intDispF >==< mapF fac >==< intInputF

   where intInputF = inputDoneSP >^^=< intF  

fac 0 = 1
fac n = n * fac (n-1)


--------------------------------------------
-- 4. A program to test the factorial function
-- (version 2, improved layout)

main4 = fudlogue (shellF "Fac" facF')

facF' = placerF (revP verticalP) (
        ("x! =" `labLeftOfF` intDispF) >==<
        mapF fac >==<
        ("x =" `labLeftOfF` intInputF))

   where intInputF = inputDoneSP >^^=< intF


--------------------------------------------
-- 5. An Up Counter 

main5 = fudlogue (shellF "Up Counter" counterF)

counterF = intDispF >==< mapstateF count 0 >==< buttonF "Up"
    where mapstateF f x = absF (mapstateSP f x)

count n Click = (n+1,[n+1])


--------------------------------------------
-- 6. An Up/Down Counter 

main6 = fudlogue (shellF "Up/Down Counter" counterF')

counterF' = intDispF >==< mapstateF count' 0 >==<
            (buttonF "Up" >+< buttonF "Down")

   where mapstateF f x = absF (mapstateSP f x)

count' n (Left Click) = (n+1,[n+1])
count' n (Right Click) = (n-1,[n-1])


--------------------------------------------
-- 7. An Up/Down/Reset Counter 

main7 = fudlogue (shellF "Up/Down/Reset Counter" counterF'')

counterF'' = intDispF >==< mapstateF count'' 0 >==< buttonsF

    where mapstateF f x = absF (mapstateSP f x)

data Buttons = Up | Down | Reset
instance Eq Buttons where a == b = show' a == show' b

buttonsF = listF [(Up,buttonF "Up"),
                  (Down,buttonF "Down"),
                  (Reset,buttonF "Reset")]

count'' n (Up,Click) = (n+1,[n+1])
count'' n (Down,Click) = (n-1,[n-1])
count'' n (Reset,Click) = (0,[0])


--------------------------------------------
-- 8. A simple pocket calculator 

main8 = fudlogue (shellF "Pocket Calculator" calcF)

calcF = intDispF >==< mapstateF calc [0] >==< buttonsF'

   where mapstateF f x = absF (mapstateSP f x)

data Buttons' = Plus | Minus | Times | Div | Entr | Digit Int
instance Eq Buttons' where a == b = show' a == show' b

buttonsF' = placerF (matrixP 4) (
              listF [d 7, d 8, d 9,op Div,
                     d 4, d 5, d 6,op Times,
                     d 1, d 2, d 3,op Minus,
                     hole,d 0,ent, op Plus])
  where
    d n = (Digit n,buttonF (show n))
    ent = op Entr
    hole = (Entr,holeF)
    op o = (o,buttonF (opLabel o))
      where opLabel Plus = "+"
            opLabel Minus = "-"
            opLabel Times  = "*"
            opLabel Div = "/"
            opLabel Entr = "Ent"

    holeF = nullLF
            

calc (n:s)   (Digit d,_) = new (n*10+d) s
calc s       (Entr,_)    = (0:s,[])
calc (y:x:s) (Plus,_)    = new (x+y) s
calc (y:x:s) (Minus,_)   = new (x-y) s
calc (y:x:s) (Times,_)   = new (x*y) s
calc (y:x:s) (Div,_)     = new (x `div` y) s
calc s       _           = (s,[])

new n s = (n:s,[n])


--------------------------------------------
-- Example from the article about Fudgets,
-- by M. Carlsson and Th. Hallgren.
--------------------------------------------

-- Cycle typing

art1 = fudgeTk (shellF "Cycle Typing" cycleType)

cycleType :: F String String
cycleType = loopF (strip >=< stringF >=< strip >=< stringF)


--------------------------------------------
-- Examples from the Fudgets-ftp-site.
--------------------------------------------

-- loop

ftp1 = fudlogue (shellF "String Loop" $ tstF)

tstF = dispF>==<loopF(strF>==<strF>==<strF)

strF = "In" `labLeftOfF` (stripInputMsg >=<stringF)

dispF = "Out" `labLeftOfF` displayF

-- stopwatch

ftp2 = fudlogue (shellF "Stop Watch" stopWatchF)

stopWatchF = 
  (timeDispF >==< (counterSP 0 >^^=< idRightF (timerF >=^< timeprep))
   >==< (runF >+< resetF)
  ) >+<quitButtonF

timeprep True = Just (100,100)
timeprep False = Nothing

runF = toggleButtonF "Run"
resetF = buttonF "Reset"

timeDispF = intDispF  -- to be improved

counterSP no = getSP $ \msg -> let out n = putSP [n] $ counterSP n 
                               in case msg of
                                    Left _ -> out (no+1)
                                    Right _ -> out 0

--------------------------------------------
-- My own examples.

-- calculator

own1 = fudgeTk (shellF "Calculator" calcuF)

calcuF :: F (Char, Click) a
calcuF = intDispF
     >=< calcuSP (0, 0, id)
     >=< (placerF (matrixP 4) . listF . map buttF)
           [ '7', '8', '9', '*'
           , '4', '5', '6', '/'
           , '1', '2', '3', '-'
           , 'C', '0', '=', '+'
           ]
  where
    buttF c = (c, buttonF [c])

calcuSP :: (Int, Int, Int -> Int) -> SP (Char, a) Int
calcuSP s = (`mapstateSP` s) $ \(d, n, f) (butt, _) -> let
  fun g = let n' = f d in ((n', 0, g n'), [n'])
 in case butt of
   '*' -> fun (*)
   '/' -> fun (/)
   '-' -> fun (-)
   '+' -> fun (+)
   '=' -> ((f d, 0, id), [f d])
   'C' -> ((0, 0, id), [0])
   c   -> let n' = n * 10 + ord c - ord '0' in ((n', n', f), [n'])

-- spinner

own2 = fudgeTk (shellF "Spinner!" spinF)

spinF :: F Int Int
spinF = placerF horizontalP
      $ loopF ( inputDoneSP >=< intF
            >=< coreSP 0
            >=< (mapF const >*< butsF)
              )

butsF :: F a (Int -> Int)
butsF = placerF verticalP
      $ (const succ >=< buttonF "+" >=< nullF)
    >*< (const pred >=< buttonF "-" >=< nullF)

coreSP :: Int -> SP (Int -> Int) Int
coreSP = stateSP ($)

succ n = n+1
pred n = n-1

--------------------------------------------
-- the end.

