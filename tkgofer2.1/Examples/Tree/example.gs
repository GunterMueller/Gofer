
-- variable node types 

data VarType = I Int | S String | T ()

instance GraphNode VarType where
  nodeWidth (I i)   = nodeWidth i
  nodeWidth (S s)   = nodeWidth s
  nodeWidth (T s)   = nodeWidth s
  nodeHeight (I i)  = nodeHeight i
  nodeHeight (S s)  = nodeHeight s
  nodeHeight (T s)  = nodeHeight s
  showGraphNode n@(I i)  (mx,my) (w,h) c = showGraphNode i (mx,my) (w,h) c
  showGraphNode n@(S s)  (mx,my) (w,h) c = showGraphNode s (mx,my) (w,h) c
  showGraphNode n@(T ()) (mx,my) (w,h) c = showGraphNode () (mx,my) (w,h) c

-- nodes with events 

data ClickNode a = Click (a, GUI ())

instance (Text a, GraphNode a) => GraphNode (ClickNode a) where
  nodeWidth (Click (n,_))  = nodeWidth n
  nodeHeight (Click (n,_)) = nodeHeight n
  showGraphNode (Click (n,a)) (mx,my) (w,h) c  =
    do let width  = nodeWidth n 
           height = nodeHeight n
           whalf  = width `div` 2
           hhalf  = height `div` 2
       void (ctext (mx-2,my) [text (show n), on (click 1) a] c) 
       result ((mx,my-hhalf),(mx,my+hhalf))
 

-- rose trees

data Rose a = R a [Rose a]

instance GraphTree Rose a where
  getSubtrees (R _ ts) = ts
  getGraphNode  (R x _ ) = x

tRose :: Rose VarType
tRose = 
  (R (I 1) [R (I 2) [],R (I 3) [R (S "Two line \nString ") []],
   R (I 4) [R (S "Hello!") [R (I 6) [R (S "One Line String ") []]],
   R (I 7) [R (S "Small") []], R (I 8) []]])

cTree :: Rose (ClickNode String)
cTree = R (Click ("Press Me To Quit!", quit)) [ R (Click ("not me!", done)) []
                                              , R (Click ("or me", done)) []
                                              ]

-- example 3 ----

-- bintrees

data BinTree a = NilTree | BNode a (BinTree a) (BinTree a)

instance GraphTree BinTree a where
  getGraphNode (BNode x l r) = x

  getSubtrees (BNode x NilTree NilTree) = []
  getSubtrees (BNode x NilTree r)       = [r]
  getSubtrees (BNode x l NilTree)       = [l]
  getSubtrees (BNode x l r      )       = [l,r]


tBin1 = BNode (S "Line 1 \nand Line 2 \nand Line 3")
          (BNode (T ()) NilTree NilTree) (BNode (I 5) NilTree NilTree)

tBin2 = BNode (I 8) (BNode (T ()) tBin1 NilTree) tBin1

tBin3 = BNode 5 (BNode 7 NilTree (BNode 2 (BNode 8 NilTree NilTree) 
           (BNode 9 NilTree NilTree))) NilTree

-- ---------------------------------------------------------------------


main :: IO ()
main = start $ 
 do w   <- windowDefault [] [background "white"]
    stR <- showTree [ treeLineColor "red"
                    , pady 20
                    , padx 20
                    , treeLineWidth 2
                    , width 200
                    , initValue tRose
                    ] w
    stB1 <- showTree [ initValue tBin2 ] w
    stB2 <- showTree [ initValue tBin3 , width 100, height 100] w
    stB3 <- showTree [ height 100, pady 20, initValue cTree ] w

    pack ((stR <*+< stB1) ^-^ flexible (stB2 <*+< stB3 ) )


  
