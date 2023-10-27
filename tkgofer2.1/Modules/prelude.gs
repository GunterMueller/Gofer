----------------------------------------------------------------------
-- TkGofer v. 2.0
-- Ton Vullinghs, Koen Claessen, July 1997
----------------------------------------------------------------------
-- prelude.gs

-- The majority of this file is taken from the Gofer cc prelude:

--         __________   __________   __________   __________   ________
--        /  _______/  /  ____   /  /  _______/  /  _______/  /  ____  \
--       /  / _____   /  /   /  /  /  /______   /  /______   /  /___/  /
--      /  / /_   /  /  /   /  /  /  _______/  /  _______/  /  __   __/
--     /  /___/  /  /  /___/  /  /  /         /  /______   /  /  \  \ 
--    /_________/  /_________/  /__/         /_________/  /__/    \__\
--
--    Functional programming environment, Version 2.30
--    Copyright Mark P Jones 1991-1994.

help = "press :? for a list of commands"

-- Operator precedence table: -----------------------------------------------

infixl 9 !!
infixr 9 ., @@
infixr 8 ^
infixl 7 *
infix  7 /, `div`, `quot`, `rem`, `mod`
infixl 6 +, -
infix  5 \\
infixr 5 ++, :
infix  4 ==, /=, <, <=, >=, >
infix  4 `elem`, `notElem`
infixr 3 &&
infixr 2 ||
infixr 0 $

-- Standard combinators: ----------------------------------------------------

primitive strict "primStrict" :: (a -> b) -> a -> b

const          :: a -> b -> a
const k x       = k

id             :: a -> a
id    x         = x

curry          :: ((a,b) -> c) -> a -> b -> c
curry f a b     =  f (a,b)

uncurry        :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

fst            :: (a,b) -> a
fst (x,_)       = x

snd            :: (a,b) -> b
snd (_,y)       = y

fst3           :: (a,b,c) -> a
fst3 (x,_,_)    = x

snd3           :: (a,b,c) -> b
snd3 (_,x,_)    = x

thd3           :: (a,b,c) -> c
thd3 (_,_,x)    = x

(.)	       :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x       = f (g x)

flip           :: (a -> b -> c) -> b -> a -> c
flip  f x y     = f y x

($)            :: (a -> b) -> a -> b     -- pronounced as `apply' elsewhere
f $ x           = f x

-- Boolean functions: -------------------------------------------------------

(&&), (||)     :: Bool -> Bool -> Bool
False && x      = False
True  && x      = x

False || x      = x
True  || x      = True

not            :: Bool -> Bool
not True        = False
not False       = True

and, or        :: [Bool] -> Bool
and             = foldr (&&) True
or              = foldr (||) False

any, all       :: (a -> Bool) -> [a] -> Bool
any p           = or  . map p
all p           = and . map p

otherwise      :: Bool
otherwise       = True

-- Character functions: -----------------------------------------------------

primitive ord "primCharToInt" :: Char -> Int
primitive chr "primIntToChar" :: Int -> Char

isAscii, isControl, isPrint, isSpace            :: Char -> Bool
isUpper, isLower, isAlpha, isDigit, isAlphanum  :: Char -> Bool

isAscii c     =  ord c < 128

isControl c   =  c < ' '    ||  c == '\DEL'

isPrint c     =  c >= ' '   &&  c <= '~'

isSpace c     =  c == ' '   || c == '\t'  || c == '\n'  || c == '\r'  ||
                               c == '\f'  || c == '\v'

isUpper c     =  c >= 'A'   &&  c <= 'Z'
isLower c     =  c >= 'a'   &&  c <= 'z'

isAlpha c     =  isUpper c  ||  isLower c
isDigit c     =  c >= '0'   &&  c <= '9'
isAlphanum c  =  isAlpha c  ||  isDigit c


toUpper, toLower      :: Char -> Char

toUpper c | isLower c  = chr (ord c - ord 'a' + ord 'A')
          | otherwise  = c

toLower c | isUpper c  = chr (ord c - ord 'A' + ord 'a')
          | otherwise  = c

minChar, maxChar      :: Char
minChar                = chr 0
maxChar                = chr 255

-- Standard type classes: ---------------------------------------------------

class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y      = not (x == y)

class Eq a => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    x <  y            = x <= y && x /= y
    x >= y            = y <= x
    x >  y            = y < x

    max x y | x >= y  = x
            | y >= x  = y
    min x y | x <= y  = x
            | y <= x  = y

class Ord a => Ix a where
    range   :: (a,a) -> [a]
    index   :: (a,a) -> a -> Int
    inRange :: (a,a) -> a -> Bool

class Ord a => Enum a where
    enumFrom       :: a -> [a]              -- [n..]
    enumFromThen   :: a -> a -> [a]         -- [n,m..]
    enumFromTo     :: a -> a -> [a]         -- [n..m]
    enumFromThenTo :: a -> a -> a -> [a]    -- [n,n'..m]

    enumFromTo n m        = takeWhile (m>=) (enumFrom n)
    enumFromThenTo n n' m = takeWhile ((if n'>=n then (>=) else (<=)) m)
                                      (enumFromThen n n')

class (Eq a, Text a) => Num a where         -- simplified numeric class
    (+), (-), (*), (/) :: a -> a -> a
    negate             :: a -> a
    fromInteger	       :: Int -> a

-- Type class instances: ----------------------------------------------------

primitive primEqInt    "primEqInt",
	  primLeInt    "primLeInt"   :: Int -> Int -> Bool
primitive primPlusInt  "primPlusInt",
	  primMinusInt "primMinusInt",
	  primDivInt   "primDivInt",
	  primMulInt   "primMulInt"  :: Int -> Int -> Int
primitive primNegInt   "primNegInt"  :: Int -> Int

instance Eq ()  where () == () = True
instance Ord () where () <= () = True

instance Eq Int  where (==) = primEqInt

instance Ord Int where (<=) = primLeInt

instance Ix Int where
    range (m,n)      = [m..n]
    index b@(m,n) i
       | inRange b i = i - m
       | otherwise   = error "index out of range"
    inRange (m,n) i  = m <= i && i <= n

instance Enum Int where
    enumFrom n       = iterate (1+) n
    enumFromThen n m = iterate ((m-n)+) n

instance Num Int where
    (+)           = primPlusInt
    (-)           = primMinusInt
    (*)           = primMulInt
    (/)           = primDivInt
    negate        = primNegInt
    fromInteger x = x

{- PC version off -}
primitive primEqFloat    "primEqFloat",
          primLeFloat    "primLeFloat"    :: Float -> Float -> Bool
primitive primPlusFloat  "primPlusFloat", 
          primMinusFloat "primMinusFloat", 
          primDivFloat   "primDivFloat",
          primMulFloat   "primMulFloat"   :: Float -> Float -> Float 
primitive primNegFloat   "primNegFloat"   :: Float -> Float
primitive primIntToFloat "primIntToFloat" :: Int -> Float

instance Eq Float where (==) = primEqFloat

instance Ord Float where (<=) = primLeFloat

instance Enum Float where
    enumFrom n       = iterate (1.0+) n
    enumFromThen n m = iterate ((m-n)+) n

instance Num Float where
    (+)         = primPlusFloat
    (-)         = primMinusFloat
    (*)         = primMulFloat
    (/)         = primDivFloat 
    negate      = primNegFloat
    fromInteger = primIntToFloat

primitive sin "primSinFloat",  asin  "primAsinFloat",
          cos "primCosFloat",  acos  "primAcosFloat",
	  tan "primTanFloat",  atan  "primAtanFloat",
          log "primLogFloat",  log10 "primLog10Float",
	  exp "primExpFloat",  sqrt  "primSqrtFloat" :: Float -> Float
primitive atan2    "primAtan2Float" :: Float -> Float -> Float
primitive truncate "primFloatToInt" :: Float -> Int

pi :: Float
pi  = 3.1415926535

{- PC version on -}

primitive primEqChar   "primEqChar",
	  primLeChar   "primLeChar"  :: Char -> Char -> Bool

instance Eq Char  where (==) = primEqChar   -- c == d  =  ord c == ord d

instance Ord Char where (<=) = primLeChar   -- c <= d  =  ord c <= ord d

instance Ix Char where
    range (c,c')      = [c..c']
    index b@(m,n) i
       | inRange b i  = ord i - ord m
       | otherwise    = error "index out of range"
    inRange (c,c') ci = ord c <= i && i <= ord c' where i = ord ci

instance Enum Char where
    enumFrom c        = map chr [ord c .. ord maxChar]
    enumFromThen c c' = map chr [ord c, ord c' .. ord lastChar]
                        where lastChar = if c' < c then minChar else maxChar

instance Eq a => Eq [a] where
    []     == []     =  True
    []     == (y:ys) =  False
    (x:xs) == []     =  False
    (x:xs) == (y:ys) =  x==y && xs==ys

instance Ord a => Ord [a] where
    []     <= _      =  True
    (_:_)  <= []     =  False
    (x:xs) <= (y:ys) =  x<y || (x==y && xs<=ys)

instance (Eq a, Eq b) => Eq (a,b) where
    (x,y) == (u,v)  =  x==u && y==v

instance (Ord a, Ord b) => Ord (a,b) where
    (x,y) <= (u,v)  = x<u  ||  (x==u && y<=v)

instance Eq Bool where
    True  == True   =  True
    False == False  =  True
    _     == _      =  False

instance Ord Bool where
    False <= x      = True
    True  <= x      = x

-- Standard numerical functions: --------------------------------------------

primitive div    "primDivInt",
	  quot   "primQuotInt",
          rem    "primRemInt",
          mod    "primModInt"    :: Int -> Int -> Int

subtract  :: Num a => a -> a -> a
subtract   = flip (-)

even, odd :: Int -> Bool
even x     = x `rem` 2 == 0
odd        = not . even

gcd       :: Int -> Int -> Int
gcd x y    = gcd' (abs x) (abs y)
             where gcd' x 0 = x
                   gcd' x y = gcd' y (x `rem` y)

lcm       :: Int -> Int -> Int
lcm _ 0    = 0
lcm 0 _    = 0
lcm x y    = abs ((x `quot` gcd x y) * y)

(^)       :: Num a => a -> Int -> a
x ^ 0      = fromInteger 1
x ^ (n+1)  = f x n x
             where f _ 0 y = y
                   f x n y = g x n where
                             g x n | even n    = g (x*x) (n`quot`2)
                                   | otherwise = f x (n-1) (x*y)

abs                     :: (Num a, Ord a) => a -> a
abs x | x>=fromInteger 0 = x
      | otherwise        = -x

signum			:: (Num a, Ord a) => a -> Int
signum x
      | x==fromInteger 0 = 0
      | x> fromInteger 0 = 1
      | otherwise        = -1

sum, product    :: Num a => [a] -> a
sum              = foldl' (+) (fromInteger 0)
product          = foldl' (*) (fromInteger 1)

sums, products	:: Num a => [a] -> [a]
sums             = scanl (+) (fromInteger 0)
products         = scanl (*) (fromInteger 1)

-- Constructor classes: -----------------------------------------------------

class Functor f where
    map :: (a -> b) -> (f a -> f b)

class Functor m => Monad m where
    result    :: a -> m a
    join      :: m (m a) -> m a
    bind      :: m a -> (a -> m b) -> m b

    join x     = bind x id
    x `bind` f = join (map f x)

class Monad m => Monad0 m where
    zero   :: m a

class Monad0 c => MonadPlus c where
    (++) :: c a -> c a -> c a

class (Functor left, Functor right) => Adjoint left right where
    univ    :: (a -> right b) -> (left a -> b)
    unit    :: a -> right (left a)
    couniv  :: (left a -> b) -> (a -> right b)
    counit  :: left (right a) -> a

    unit     = couniv id
    counit   = univ id
    univ g   = counit . map g
    couniv g = map g . unit

class (Functor f, Functor g) => NatTransf f g where
    eta :: f a -> g a

-- Monad based utilities: ---------------------------------------------------

apply            :: Monad m => (a -> m b) -> (m a -> m b)
apply             = flip bind

(@@)             :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
f @@ g            = join . map f . g

concat		 :: MonadPlus c => [c a] -> c a
concat		  = foldr (++) zero

filter		 :: Monad0 m => (a -> Bool) -> m a -> m a
filter p xs       = [ x | x<-xs, p x ]

mfoldl           :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
mfoldl f a []     = result a
mfoldl f a (x:xs) = f a x `bind` (\fax -> mfoldl f fax xs)

mfoldr           :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
mfoldr f a []     = result a
mfoldr f a (x:xs) = mfoldr f a xs `bind` (\y -> f x y)

mapl             :: Monad m => (a -> m b) -> ([a] -> m [b])
mapl f []         = [ [] ]
mapl f (x:xs)     = [ y:ys | y <- f x, ys <- mapl f xs ]

mapr             :: Monad m => (a -> m b) -> ([a] -> m [b])
mapr f []         = [ [] ]
mapr f (x:xs)     = [ y:ys | ys <- mapr f xs, y <- f x ]

-- The monad of lists: ------------------------------------------------------

instance Functor   [] where map f []     = []
			    map f (x:xs) = f x : map f xs

instance Monad     [] where result x        = [x]
			    []     `bind` f = []
			    (x:xs) `bind` f = f x ++ (xs `bind` f)

instance Monad0    [] where zero         = []

instance MonadPlus [] where []     ++ ys = ys
			    (x:xs) ++ ys = x : (xs ++ ys)

-- Standard list processing functions: --------------------------------------

head             :: [a] -> a
head (x:_)        = x

last             :: [a] -> a
last [x]          = x
last (_:xs)       = last xs

tail             :: [a] -> [a]
tail (_:xs)       = xs

init             :: [a] -> [a]
init [x]          = []
init (x:xs)       = x : init xs

genericLength    :: Num a => [b] -> a    -- calculate length of list
genericLength     = foldl' (\n _ -> n + fromInteger 1) (fromInteger 0)

length		 :: [a] -> Int
length            = foldl' (\n _ -> n + 1) 0

(!!)             :: [a] -> Int -> a      -- xs!!n selects the nth element of
(x:_)  !! 0       = x                    -- the list xs (first element xs!!0)
(_:xs) !! (n+1)   = xs !! n              -- for any n < length xs.

iterate          :: (a -> a) -> a -> [a] -- generate the infinite list
iterate f x       = x : iterate f (f x)  -- [x, f x, f (f x), ...

repeat           :: a -> [a]             -- generate the infinite list
repeat x          = xs where xs = x:xs   -- [x, x, x, x, ...

cycle            :: [a] -> [a]           -- generate the infinite list
cycle xs          = xs' where xs'=xs++xs'-- xs ++ xs ++ xs ++ ...

copy             :: Int -> a -> [a]      -- make list of n copies of x
copy n x          = take n xs where xs = x:xs

nub              :: Eq a => [a] -> [a]   -- remove duplicates from list
nub []            = []
nub (x:xs)        = x : nub (filter (x/=) xs)

reverse          :: [a] -> [a]           -- reverse elements of list
reverse           = foldl (flip (:)) []

elem, notElem    :: Eq a => a -> [a] -> Bool
elem              = any . (==)           -- test for membership in list
notElem           = all . (/=)           -- test for non-membership

maximum, minimum :: Ord a => [a] -> a
maximum           = foldl1 max          -- max element in non-empty list
minimum           = foldl1 min          -- min element in non-empty list

transpose        :: [[a]] -> [[a]]      -- transpose list of lists
transpose         = foldr
                      (\xs xss -> zipWith (:) xs (xss ++ repeat []))
                      []

-- null provides a simple and efficient way of determining whether a given
-- list is empty, without using (==) and hence avoiding a constraint of the
-- form Eq [a].

null             :: [a] -> Bool
null []           = True
null (_:_)        = False

-- (\\) is used to remove the first occurrence of each element in the second
-- list from the first list.  It is a kind of inverse of (++) in the sense
-- that  (xs ++ ys) \\ xs = ys for any finite list xs of proper values xs.

(\\)             :: Eq a => [a] -> [a] -> [a]
(\\)              = foldl del
                    where []     `del` _  = []
                          (x:xs) `del` y
                             | x == y     = xs
                             | otherwise  = x : xs `del` y

-- Fold primitives:  The foldl and scanl functions, variants foldl1 and
-- scanl1 for non-empty lists, and strict variants foldl' scanl' describe
-- common patterns of recursion over lists.  Informally:
--
--  foldl f a [x1, x2, ..., xn]  = f (...(f (f a x1) x2)...) xn
--                               = (...((a `f` x1) `f` x2)...) `f` xn
-- etc...
--
-- The functions foldr, scanr and variants foldr1, scanr1 are duals of these
-- functions:
-- e.g.  foldr f a xs = foldl (flip f) a (reverse xs)  for finite lists xs.

foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl f z []      = z
foldl f z (x:xs)  = foldl f (f z x) xs

foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)   = foldl f x xs

foldl'           :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     =  a
foldl' f a (x:xs) =  strict (foldl' f) (f a x) xs

scanl            :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs      = q : (case xs of
                         []   -> []
                         x:xs -> scanl f (f q x) xs)

scanl1           :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)   = scanl f x xs

scanl'           :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f q xs     = q : (case xs of
                         []   -> []
                         x:xs -> strict (scanl' f) (f q x) xs)

foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr f z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

foldr1           :: (a -> a -> a) -> [a] -> a
foldr1 f [x]      = x
foldr1 f (x:xs)   = f x (foldr1 f xs)

scanr            :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
                    where qs@(q:_) = scanr f q0 xs

scanr1           :: (a -> a -> a) -> [a] -> [a]
scanr1 f [x]      = [x]
scanr1 f (x:xs)   = f x q : qs
                    where qs@(q:_) = scanr1 f xs

-- List breaking functions:
--
--   take n xs       returns the first n elements of xs
--   drop n xs       returns the remaining elements of xs
--   splitAt n xs    = (take n xs, drop n xs)
--
--   takeWhile p xs  returns the longest initial segment of xs whose
--                   elements satisfy p
--   dropWhile p xs  returns the remaining portion of the list
--   span p xs       = (takeWhile p xs, dropWhile p xs)
--
--   takeUntil p xs  returns the list of elements upto and including the
--                   first element of xs which satisfies p

take                :: Int -> [a] -> [a]
take 0     _         = []
take _     []        = []
take (n+1) (x:xs)    = x : take n xs

drop                :: Int -> [a] -> [a]
drop 0     xs        = xs
drop _     []        = []
drop (n+1) (_:xs)    = drop n xs

splitAt             :: Int -> [a] -> ([a], [a])
splitAt 0     xs     = ([],xs)
splitAt _     []     = ([],[])
splitAt (n+1) (x:xs) = (x:xs',xs'') where (xs',xs'') = splitAt n xs

takeWhile           :: (a -> Bool) -> [a] -> [a]
takeWhile p []       = []
takeWhile p (x:xs)
         | p x       = x : takeWhile p xs
         | otherwise = []

takeUntil           :: (a -> Bool) -> [a] -> [a]
takeUntil p []       = []
takeUntil p (x:xs)
       | p x         = [x]
       | otherwise   = x : takeUntil p xs

dropWhile           :: (a -> Bool) -> [a] -> [a]
dropWhile p []       = []
dropWhile p xs@(x:xs')
         | p x       = dropWhile p xs'
         | otherwise = xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p xs@(x:xs')
         | p x       = let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise = ([],xs)
break p              = span (not . p)

-- Text processing:
--   lines s     returns the list of lines in the string s.
--   words s     returns the list of words in the string s.
--   unlines ls  joins the list of lines ls into a single string
--               with lines separated by newline characters.
--   unwords ws  joins the list of words ws into a single string
--               with words separated by spaces.

lines     :: String -> [String]
lines ""   = []
lines s    = l : (if null s' then [] else lines (tail s'))
             where (l, s') = break ('\n'==) s

words     :: String -> [String]
words s    = case dropWhile isSpace s of
                  "" -> []
                  s' -> w : words s''
                        where (w,s'') = break isSpace s'

unlines   :: [String] -> String
unlines    = concat . map (\l -> l ++ "\n")

unwords   :: [String] -> String
unwords [] = []
unwords ws = foldr1 (\w s -> w ++ ' ':s) ws

-- Merging and sorting lists:

merge               :: Ord a => [a] -> [a] -> [a] 
merge []     ys      = ys
merge xs     []      = xs
merge (x:xs) (y:ys)
        | x <= y     = x : merge xs (y:ys)
        | otherwise  = y : merge (x:xs) ys

sort                :: Ord a => [a] -> [a]
sort                 = foldr insert []

insert              :: Ord a => a -> [a] -> [a]
insert x []          = [x]
insert x (y:ys)
        | x <= y     = x:y:ys
        | otherwise  = y:insert x ys

qsort               :: Ord a => [a] -> [a]
qsort []             = []
qsort (x:xs)         = qsort [ u | u<-xs, u<x ] ++
                             [ x ] ++
                       qsort [ u | u<-xs, u>=x ]

-- zip and zipWith families of functions:

zip  :: [a] -> [b] -> [(a,b)]
zip   = zipWith  (\a b -> (a,b))

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3  = zipWith3 (\a b c -> (a,b,c))

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4  = zipWith4 (\a b c d -> (a,b,c,d))

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5  = zipWith5 (\a b c d e -> (a,b,c,d,e))

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a,b,c,d,e,f)]
zip6  = zipWith6 (\a b c d e f -> (a,b,c,d,e,f))

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
zip7  = zipWith7 (\a b c d e f g -> (a,b,c,d,e,f,g))


zipWith                  :: (a->b->c) -> [a]->[b]->[c]
zipWith z (a:as) (b:bs)   = z a b : zipWith z as bs
zipWith _ _      _        = []

zipWith3                 :: (a->b->c->d) -> [a]->[b]->[c]->[d]
zipWith3 z (a:as) (b:bs) (c:cs)
                          = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _          = []

zipWith4                 :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                          = z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _        = []

zipWith5                 :: (a->b->c->d->e->f) -> [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                          = z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _      = []

zipWith6                 :: (a->b->c->d->e->f->g)
                            -> [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                          = z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _    = []

zipWith7                 :: (a->b->c->d->e->f->g->h)
                             -> [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                          = z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _  = []

unzip                    :: [(a,b)] -> ([a],[b])
unzip                     = foldr (\(a,b) ~(as,bs) -> (a:as, b:bs)) ([], [])

-- Formatted output: --------------------------------------------------------

primitive primPrint "primPrint"  :: Int -> a -> String -> String

show'       :: a -> String
show' x      = primPrint 0 x []

cjustify, ljustify, rjustify :: Int -> String -> String

cjustify n s = space halfm ++ s ++ space (m - halfm)
               where m     = n - length s
                     halfm = m `div` 2
ljustify n s = s ++ space (n - length s)
rjustify n s = space (n - length s) ++ s

space       :: Int -> String
space n      = copy n ' '

layn        :: [String] -> String
layn         = lay 1 where lay _ []     = []
                           lay n (x:xs) = rjustify 4 (show n) ++ ") "
                                           ++ x ++ "\n" ++ lay (n+1) xs

-- Miscellaneous: -----------------------------------------------------------

until                  :: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x       = x
            | otherwise = until p f (f x)

until'                 :: (a -> Bool) -> (a -> a) -> a -> [a]
until' p f              = takeUntil p . iterate f

primitive error "primError" :: String -> a

undefined              :: a
undefined | False       = undefined

asTypeOf               :: a -> a -> a
x `asTypeOf` _          = x

-- A trimmed down version of the Haskell Text class: ------------------------

type  ShowS   = String -> String

class Text a where 
    showsPrec      :: Int -> a -> ShowS
    showList       :: [a] -> ShowS

    showsPrec       = primPrint
    showList []     = showString "[]"
    showList (x:xs) = showChar '[' . shows x . showl xs
                      where showl []     = showChar ']'
                            showl (x:xs) = showChar ',' . shows x . showl xs

shows      :: Text a => a -> ShowS
shows       = showsPrec 0

show       :: Text a => a -> String
show x      = shows x ""

showChar   :: Char -> ShowS
showChar    = (:)

showString :: String -> ShowS
showString  = (++)

instance Text () where
    showsPrec d ()    = showString "()"

instance Text Bool where
    showsPrec d True  = showString "True"
    showsPrec d False = showString "False"

primitive primShowsInt "primShowsInt" :: Int -> Int -> String -> String
instance Text Int where showsPrec = primShowsInt

{- PC version off -}
primitive primShowsFloat "primShowsFloat" :: Int -> Float -> String -> String
instance Text Float where showsPrec = primShowsFloat
{- PC version on -}

instance Text Char where
    showsPrec p c = showString [q, c, q] where q = '\''
    showList cs   = showChar '"' . showl cs
                    where showl ""       = showChar '"'
                          showl ('"':cs) = showString "\\\"" . showl cs
                          showl (c:cs)   = showChar c . showl cs
			  -- Haskell has   showLitChar c . showl cs

instance Text a => Text [a]  where
    showsPrec p = showList

instance (Text a, Text b) => Text (a,b) where
    showsPrec p (x,y) = showChar '(' . shows x . showChar ',' .
                                       shows y . showChar ')'

-- (Dialogue functions removed) --------------------------------------------

primitive primFopen "primFopen" :: String -> a -> (String -> a) -> a

openfile        :: String -> String
openfile f       = primFopen f (error ("can't open file "++f)) id

-- End of Gofer standard prelude --------------------------------------------

----------------------------------------------------------------------
-- Basic extensions

--------------------------------------------------------------------------
-- General Functions
 
-- numbers

numval :: String -> Int
numval (' ':xs) = numval xs
numval ('+':xs) = numval xs
numval ('-':xs) = - (numval xs)
numval xs       = foldl (\n x -> 10 * n + ord x - ord '0') 0 xs

-- strings

infix 4 `isPrefix`, `isRealPrefix`

isPrefix :: Eq a => [a] -> [a] -> Bool
[]    `isPrefix` _     = True
(a:s) `isPrefix` (b:t) = a == b && s `isPrefix` t
_     `isPrefix` []    = False

isRealPrefix :: Eq a => [a] -> [a] -> Bool
_     `isRealPrefix` []    = False
(a:s) `isRealPrefix` (b:t) = a == b && s `isRealPrefix` t
[]    `isRealPrefix` _     = True

-- self

self :: (a -> a -> b) -> a -> b
self f x = f x x

-- grouping

groupn :: Int -> [a] -> [[a]]
groupn n = takeWhile (not.null) . map (take n) . iterate (drop n)

--------------------------------------------------------------------------
-- General Monadic Functions
 

done :: Monad m => m ()
done = result ()
 
infixr 1 ==>

(==>) :: Monad m => m Bool -> m a -> m ()
b ==> a = do { c <- b; if c then void a else done }
 
seq :: Monad m => m a -> m b -> m b
seq am bm = do { am; bm }

void :: Monad m => m a -> m ()
void m = do { m; done }
 
binds :: Monad m => [m a] -> m [a]
binds = mapl id

seqs :: Monad m => [m ()] -> m ()
seqs = foldr seq done

exitM :: Monad m => String -> m a
exitM s = error (copy 15 '\b' ++ s ++ copy (max 0 (15 - length s)) ' ')

ifM :: Monad m => Bool -> m () -> m ()
ifM b m = if b then m else done

--------------------------------------------------------------------------
-- type Maybe

data Maybe a = Just a | Nothing

instance Functor Maybe where
  map f (Just a) = Just (f a)
  map f Nothing  = Nothing

instance Monad Maybe where
  result a = Just a
  Just a  `bind` k = k a
  Nothing `bind` k = Nothing

instance Monad0 Maybe where
  zero = Nothing

instance MonadPlus Maybe where
  Nothing ++ m = m
  m       ++ _ = m

maybe :: (a -> c) -> c -> Maybe a -> c
maybe just nothing (Just a) = just a
maybe just nothing Nothing  = nothing

--------------------------------------------------------------------------
-- type Either

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either left right (Left a)  = left a
either left right (Right b) = right b

--------------------------------------------------------------------------
-- type RoseTree

data RoseTree a = Rose a [RoseTree a]

instance Functor RoseTree where
  map f (Rose a ts) = Rose (f a) (map (map f) ts)

roseTree :: (a -> [b] -> b) -> RoseTree a -> b
roseTree f (Rose a rs) = f a (map (roseTree f) rs)

--------------------------------------------------------------------------
-- IO monad

primitive bindIO       "primIOBind"    :: IO a -> (a -> IO b) -> IO b
primitive resultIO     "primSTReturn"  :: a -> ST s a
primitive primGetCh    "primIOGetch"   :: IO Char
primitive primPutChar  "primIOPutchar" :: Char -> IO ()
primitive interleaveIO "primSTInter" :: IO a -> IO a


instance Monad IO where
  bind   = bindIO
  result = resultIO

instance Monad0 IO where
  zero = error "Pattern match failed in IO-monad."

instance Functor IO where
  map f m = [ f x | x <- m ]

class Monad m => IOMonad m where
  getCh       :: m Char
  getContents :: m String

  getChar :: m Char
  getChar = do c <- getCh; putChar c; result c

  putChar :: Char -> m ()
  putChar c = putStr [c]
  
  putStr  :: String -> m ()
  putStr s = seqs [ putChar c | c <- s ]
  
instance IOMonad IO where
  getCh   = primGetCh
  putChar = primPutChar

  getContents = interleaveIO $
    do c <- getCh
       s <- getContents
       result (c:s)

print :: (Text a, IOMonad m) => a -> m ()
print x = putStrLn (show x)

putStrLn :: IOMonad m => String -> m ()
putStrLn s = do putStr s; putChar '\n'

getLine :: IOMonad m => m String
getLine  = do c <- getChar
              if c=='\n' then result ""
			 else do cs <- getLine
				 result (c:cs)

interact :: IOMonad m => (String -> String) -> m ()
interact f = do s <- getContents; putStr (f s)

--------------------------------------------------------------------------
-- MutVar's

type Var a = MutVar RealWorld a
  in newVar, readVar, writeVar, eqVar

primitive newVar   "primSTNew"      :: a -> IO (Var a)
primitive readVar  "primSTDeref"    :: Var a -> IO a
primitive writeVar "primSTAssign"   :: Var a -> a -> IO ()
primitive eqVar    "primSTMutVarEq" :: Var a -> Var a -> Bool

newVar' :: IO (Var a)
newVar' = newVar undefined

updateVar :: (a -> a) -> Var a -> IO ()
updateVar f var = do { a <- readVar var; writeVar var (f a) }

instance Eq (Var a) where
  (==) = eqVar

--------------------------------------------------------------------------
-- MutArr's

primitive primSTNewArr   "primSTNewArr"
          :: (a -> Int) -> (a,a) -> b -> ST s (MutArr s a b)
primitive primSTReadArr  "primSTReadArr"
          :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> ST s b
primitive primSTWriteArr "primSTWriteArr"
          :: ((a,a) -> a -> Int) -> MutArr s a b -> a -> b -> ST s ()

type LArray a b = MutArr RealWorld a b
  in newArr, readArr, writeArr

newArr   :: Ix a => (a,a) -> b -> IO (LArray a b)
readArr  :: Ix a => LArray a b -> a -> IO b 
writeArr :: Ix a => LArray a b -> a -> b -> IO ()

newArr bounds c  = primSTNewArr (index bounds) bounds c
readArr arr i    = primSTReadArr index arr i
writeArr arr i x = primSTWriteArr index arr i x

--------------------------------------------------------------------------
-- Heap

type HeapPtr = Int
  in showPtr, readPtr
   , newHeap, heapAlloc, heapWrite, heapFree, heapFilter, heapRead 

type Heap a = (LArray HeapPtr (Maybe a), Var (HeapPtr, [HeapPtr]))
  in newHeap, heapAlloc, heapWrite, heapFree, heapFilter, heapRead

newHeap :: Int -> IO (Heap a)
newHeap size =
  do heap <- newArr (1,size) Nothing
     free <- newVar (0 :: HeapPtr, [1..size] :: [HeapPtr])
     result (heap, free)

heapAlloc :: Heap a -> a -> IO HeapPtr
heapAlloc (arr, free) a =
  do (last, ptrs) <- readVar free
     case ptrs of
       []       -> error "Heap is full"
       (p:ptrs) -> do x <- readArr arr p
                      case x of
                        Nothing -> done
                        Just _  -> error "Full HeapCell reallocated"
                      writeArr arr p (Just a)
                      writeVar free (max p last, ptrs)
                      result p

heapWrite :: Heap a -> HeapPtr -> a -> IO ()
heapWrite (arr, free) ptr a =
  do ma <- readArr arr ptr
     case ma of
       Just _  -> writeArr arr ptr (Just a)
       Nothing -> error "Writing to an unallocated Heapcell"

heapFree :: Heap a -> HeapPtr -> IO ()
heapFree (arr, free) ptr =
  do writeArr arr ptr Nothing
     (last, ptrs) <- readVar free
     if ptr == last then
       writeVar free (last-1, ptr:ptrs)
      else
       writeVar free (last, ptr:ptrs)

heapRead :: Heap a -> HeapPtr -> IO a
heapRead (arr, free) ptr =
  do ma <- readArr arr ptr
     case ma of
       Just a  -> result a
       Nothing -> error "Reading an unallocated HeapCell"

heapFilter :: Heap a -> (a -> Bool) -> IO ()
heapFilter (arr, free) pred =
  do (last, ptrs)  <- readVar free
     (last',ptrs') <- doAll [1..last] (0,ptrs)
     writeVar free (last', ptrs')
 where
  doAll []     (last,ptrs) = result (last,ptrs)
  doAll (p:ps) (last,ptrs) =
    do ma <- readArr arr p
       case ma of
         Just a | pred a    -> do doAll ps (p,ptrs)
                | otherwise -> do writeArr arr p Nothing
                                  doAll ps (last,p:ptrs)
         Nothing            -> do doAll ps (last,ptrs)

showPtr :: HeapPtr -> String
showPtr p = 'p':show p

readPtr :: String -> HeapPtr
readPtr ('p':s) = numval s

instance Text HeapPtr where
  showsPrec _ p = showString (showPtr p)

--------------------------------------------------------------------------
-- type Dyn

data Dyn = Dyn

fromDyn :: Dyn -> a
fromDyn = typeCast

toDyn :: a -> Dyn
toDyn = typeCast

primitive primCast "primStrict" :: (a -> a) -> a -> b

typeCast :: a -> b
typeCast = primCast id

--------------------------------------------------------------------------
-- the end.

