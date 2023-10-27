-- graphic.gs : new canvas objects
-- 

-- [1] arcs and polygons --------------------------------------

main :: IO ()
main = start $
  do w   <- window [title "canvas objects"]
     c   <- canvas [background "white"] w
     arc <- carc (10,10) (100,100) 
              [angleExtent 180, angleStart 45,
               penColor "red", fillColor "yellow"
              ] c
     pol <- cpolygon [(10,10),(100,100),(50,20),(30,70)] 
                  [fillColor "green", penColor "red"] c
     pack c 


-- [2] pictures -----------------------------------------------

pooh :: IO ()
pooh = start $
  do w <- window []
     c <- canvas [background "blue" ] w
     cpicture (100,100) 
       [ image "pooh.gif"
       , on enterWidget (do cset c (background "white")
                            print "hi!"
                        )
       , on leaveWidget (cset c (background "blue"))
       ] c 
     pack c
  
 
-- [3] How to add new options? ---------------------------------

-- lookup the definition of the options in the tk manual pages
-- write a function `option' -> GUI ()


-- example 1 : the arrow option 
 
arrow :: String -> Conf CLine
arrow = myConf . tk_arrow 

tk_arrow :: String -> CLine -> GUI () 
tk_arrow kind (CItem _ n i) =
  do let cs = "-arrow " ++ kind
     tk_putTcl [n, "itemconfigure", show i, cs]


-- example 2 : the arrowShape option 

arrowShape :: (Int,Int,Int) -> Conf CLine
arrowShape = myConf . tk_arrowShape

tk_arrowShape :: (Int,Int,Int) -> CLine -> GUI () 
tk_arrowShape (a,b,c) (CItem _ n i) = 
  do let cs = ["-arrowshape {", show a, show b, show c, "}" ]
     tk_putTcl ([n, "itemconfigure", show i] ++ cs)


-- application 

arrowLine :: IO ()
arrowLine = start $
  do w <- window []
     c <- canvas [] w
     l <- cline (30,30) (100,100) 
            [arrow "both", arrowShape (10,24,5) ] c
     pack c
     
