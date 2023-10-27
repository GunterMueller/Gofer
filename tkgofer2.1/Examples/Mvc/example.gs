--------------------------------------------------------
-- Examples for MVC

-- need mvc.gs

--------------------------------------------------------
-- Some instances for View

instance Text v => View Label v where
  updateView l v = cset l (text (show v))
  invokeView _ _ = done

instance HasInput WItem Entry0 v => View (Entry v) v where
  updateView       = setValue
  invokeView w com = cset w $ on return $ do { v <- getValue w; com v}

instance ( HasInput WItem Scale0 v
	 , HasCommand (WItem (Scale0 v))
	 ) => View (WItem (Scale0 v)) v where
  updateView       = setValue
  invokeView w com = cset w $ command $ do { v <- getValue w; com v }

---------------------------------------------------------
-- An example 

main = start $
  do c <- control [ initValue 0 ]
     copy c
 where
  copy c =
    do w <- window [ title "MVC!" ]
       s <- vscale [ mvc c ] w
       e <- entry  [ initValue 0, mvc c ] w
       l <- label  [ mvc c ] w
       b <- button [ text "Copy", command (copy c) ] w
       pack (s << (l ^^ e ^^ b))

---------------------------------------------------------
-- the end.
