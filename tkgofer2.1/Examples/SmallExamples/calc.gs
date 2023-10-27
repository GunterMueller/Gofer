
type CalcState = (Int, Int -> Int)

ex_calc :: IO ()
ex_calc = start $
  do st <- newState (0, id)
     w <- windowDefault [title "Calculator"] [font "12x24"]
     c <- calc st w
     pack c

calc :: Var CalcState -> Window -> GUI Frame
calc st w =
  let disp   = entry [relief "sunken", width 12, initValue 0] w

      keys e = map (cmd e) [ '1', '2', '3', '+',
                             '4', '5', '6', '-',
                             '7', '8', '9', '*',
                             'C', '0', '=', '/'
                           ]

      cmd e c = button [text [c], command (next e (action c)), width 2] w

      next e f = do (disp, accu)  <- readState st
                    let (disp',accu') = f (disp, accu)
                    setValue e disp'
                    writeState st (disp',accu')

      action 'C' (d,a) = (0, id)
      action '=' (d,a) = (a d, const (a d))
      action  c  (d,a) | isDigit c = (10*d + ord c - ord '0', a)
                       | otherwise = (0, ((char2op c).a) d)

      char2op '+' = (+)
      char2op '-' = (-)
      char2op '*' = (*)
      char2op '/' = \x y -> if y == 0 then 99999999 else x `div` y

  in do e <- disp
        k <- binds (keys e)
        result (e ^-^ matrix 4 k)
