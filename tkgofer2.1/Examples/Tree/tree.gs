-- 
-- Tree Widget: graphical display for arbitrary branching trees
--
-- Joachim Schmid, March 1997, Uni Ulm
--



-- ------------------------------------------------------------------------
-- specify the layout of a node

class GraphNode a where
   showGraphNode  :: a -> (Int,Int) -> (Int,Int) -> Canvas -> 
                     GUI ((Int,Int),(Int,Int))
   nodeHeight :: a -> Int
   nodeWidth  :: a -> Int


-- and create trees

class GraphNode a => GraphTree c a where
   getSubtrees   :: c a -> [c a]
   getGraphNode  :: c a -> a

-- ------------------------------------------------------------------------
-- a new widget, the showTree widget 


showTree :: ( GraphTree t a
            , Widget (ShowTree (t a))
            ) => [Conf (ShowTree (t a))] -> Window -> GUI (ShowTree (t a))
showTree cs w = do
      tw <- newGVar undefined
      c  <- canvas  [] w
      h  <- hscroll [] c
      v  <- vscroll [] c
      d  <- newGVar ((10,10),1,"black")
      sw <- composeWidget (ShowTree (ShowTreeInfo (c,h,v,d)) tw) 
                  (flexible (flexible c <|< v) ^-^ h) cs
      result sw

-- ------------------------------------------------------------------------
-- some default instances for nodes

-- Int

instance GraphNode Int where
  nodeWidth _  = 20
  nodeHeight _ = 20
  showGraphNode n (mx,my) (w,h) c  =
    do let width  = nodeWidth n
           height = nodeHeight n
           whalf  = width `div` 2
           hhalf  = height `div` 2
           sx     = mx-whalf
           sy     = my-hhalf
       void (coval (sx,sy) (sx+height, sy+height) [] c)    -- a circle
       void (ctext (mx-2,my) [text (show n)] c)            -- and its contents
       result ((mx,my-hhalf),(mx,my+hhalf))

-- String

instance GraphNode String where
  nodeWidth s  = let ls = lines s
                 in (maximum (map length ls))*7
  nodeHeight s = let ls = lines s
                 in (length ls)*17

  showGraphNode s (mx,my) (w,h) c =
    do let width   = nodeWidth s
           height  = nodeHeight s
           whalf   = width  `div` 2
           hhalf   = height `div` 2
           sx      = mx-whalf
           sy      = my-hhalf
           ls      = lines s
       void (crect (sx,sy) (sx+width,sy+height) [] c)   -- a box
       void (ctext (sx+2,my) [text s] c)                -- and its contents
       result ((mx,my-hhalf),(mx,my+hhalf))


-- ()

instance GraphNode () where
  nodeWidth _  = 3
  nodeHeight _ = 5

  showGraphNode _ (mx,my) (w,h) c = 
    do let width   = nodeWidth ()
           height  = nodeHeight ()
           whalf   = width `div` 2
           hhalf   = height `div` 2
           sx      = mx-whalf
           sy      = my-hhalf
       void (coval (sx,sy) (sx+width,sy+width) [fillColor "red", penWidth 3] c)
       result ((mx,my-hhalf),(mx,my+hhalf))
     

-- ------------------------------------------------------------------------
-- implementation of the ShowTree widget 
-- ------------------------------------------------------------------------

data ShowTree0 a  = ShowTree ShowTreeInfo (GVar a)
type ShowTree a   = WItem (ShowTree0 a) 
data ShowTreeInfo = ShowTreeInfo 
                        ( Canvas                        -- a drawing canvas
                        , Scrollbar,Scrollbar           -- two scrollbars
                        , GVar ((Int,Int),Int,String)   -- global info
                        )
data TreeSizes a  = TreeSize ((Int,Int),Int) [TreeSizes a]

-- instance declarations 

instance GraphTree t a => HasInput WItem ShowTree0 (t a) where
   getValue (WItem (ShowTree info t) _)    = readGVar t 
   setValue (WItem (ShowTree (ShowTreeInfo (c,sh,sv,d)) t) _) t' = do
     (d',lw,cl) <- readGVar d
     let ts          = treeReorganize (treeSizes d' t')
         TreeSize ((w,h),th) _ = ts
     clearCanvas c
     cset c (scrollRegion (w+2,th+2))
     writeGVar t t'
     displayTree (lw,cl) (-1,-1) [ts] [t'] c (0,0)

instance (GraphTree t a,HasInput WItem ShowTree0 (t a)) => Widget (ShowTree (t a)) where
   cset w@(WItem (ShowTree (ShowTreeInfo (c,sh,sv,d)) t) _) cs = 
              let newc = cs w
              in case newc of
                   (Tk_InitValue s)  -> s     
                   (Tk_Height h)     -> cset c (height h)
                   (Tk_Width  w)     -> cset c (width w)
                   (Tk_Background b) -> cset c (background b)
                   (Tk_PadX i)       -> do ((dx,dy),lw,cl) <- readGVar d
                                           writeGVar d ((i,dy),lw,cl)
                   (Tk_PadY i)       -> do ((dx,dy),lw,cl) <- readGVar d
                                           writeGVar d ((dx,i),lw,cl)
                   (Tk_FreeOption act) -> act
                   otherwise           -> cset c (const newc)

instance HasPad        (ShowTree a)
instance HasBackground (ShowTree a)
instance HasWidth      (ShowTree a)
instance HasHeight     (ShowTree a)
instance HasBorder     (ShowTree a)

-- new configuration options 
 
treeLineColor :: ( Widget (ShowTree (t a))
                 , GraphTree t a )
              => String -> Conf (ShowTree (t a)) 
treeLineColor v = myConf (setColor v) where
  setColor cl (WItem (ShowTree (ShowTreeInfo (c,sh,sv,d)) t) _) 
    = do (d',lw,cl) <- readGVar d
         writeGVar d (d',lw,v)

treeLineWidth :: ( Widget (ShowTree (t a))
                 , GraphTree t a
                 ) => Int -> Conf (ShowTree (t a)) 
treeLineWidth v = myConf (setWidth v) where
  setWidth cl (WItem (ShowTree (ShowTreeInfo (c,sh,sv,d)) t) _) 
    = do (d',lw,cl) <- readGVar d
         writeGVar d (d',v,cl)

treeScroll :: ( Widget (ShowTree (t a)) 
              , GraphTree t a
              ) => Conf Scrollbar -> Conf (ShowTree (t a))
treeScroll c = myConf (setScroll c) where
 setScroll cs (WItem (ShowTree (ShowTreeInfo (c,sh,sv,d)) t) _) 
    = do cset sh cs
         cset sv cs

-- calculating tree layouts ---------------------------------------------

treeReorganize :: TreeSizes ((Int,Int),Int) -> TreeSizes ((Int,Int),Int)
treeReorganize n@(TreeSize ((w,h),th) []) = n
treeReorganize (TreeSize ((w,h),th) ts) =
     let width = sum (map (\(TreeSize ((w,_),_) _)->w) ts)
         diff  = w-width
         height= maximum (map (\(TreeSize ((_,h),_) _)->h) ts)
         ts'   = [treeReorganize (TreeSize ((conv w' width diff,height),th-h) ts'') |
                  TreeSize ((w',_),_) ts'' <- ts]
     in (TreeSize ((w,h),th) ts') where
  conv w width diff = 
    let widthF = primIntToFloat width
        diffF  = primIntToFloat diff
        wF     = primIntToFloat w
        ergF   = wF+(wF/widthF)*diffF
    in truncate ergF

treeSizes :: GraphTree t a => (Int,Int) -> t a -> TreeSizes ((Int,Int),Int)
treeSizes (dx,dy) t =
     let ts  = getSubtrees t
         tss = map (treeSizes (dx,dy)) ts
         n   = getGraphNode t
         nwidth  = (nodeWidth n)+dx
         nheight = (nodeHeight n)+dy
     in case ts of
        []  -> TreeSize ((nwidth,nheight),nheight) tss
        _   -> let widthChildren = sum (map (\(TreeSize ((w,h),_)_)->w) tss)
                   heightChildren= maximum (map 
                       (\(TreeSize ((w,h),th)_)->th-h+nheightCh) tss)
                   nheightCh     = maximum (map (\(TreeSize ((_,h),_) _)->h) tss)
               in  TreeSize ((max nwidth widthChildren,nheight), 
                             nheight+heightChildren) tss

displayTree :: GraphTree t a => (Int,String) -> (Int,Int) -> 
                   [TreeSizes ((Int,Int),Int)] -> [t a] -> Canvas -> 
                   (Int,Int) -> GUI ()
displayTree (lw,cl) fpos [] [] c (sx,sy) = result ()
displayTree (lw,cl) fpos (tS:tSs) (t:ts) c (sx,sy) =
     let TreeSize ((w,h),th) tS' = tS
         middleX   = sx+(w `div` 2)
         middleY   = sy+(h `div` 2)
         node      = getGraphNode t
         nwidth    = nodeWidth node
         nheight   = nodeHeight node
     in do 
       (s,e) <- showGraphNode node (middleX,middleY) (w,h) c
       case fpos of
        (-1,-1) -> result ()
        _       -> void (cline fpos s [penColor cl,fillColor cl,\_->Tk_Width lw] c)
       displayTree (lw,cl) fpos tSs ts c (sx+w,sy)
       displayTree (lw,cl) e tS' (getSubtrees t) c (sx,sy+h)

