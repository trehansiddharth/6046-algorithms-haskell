{-# LANGUAGE DeriveFunctor #-}

import Prelude hiding ((.), id, zip, zipWith)
import Control.Category
import Data.Zip

{--
The Arrangement data type is similar to sequence in that it provides O(lg(n)) random access, but it
is arranged differently. It has the property that an element at a larger index can never be the parent
of an element at a smaller index. This allows us to have infinitely long Arrangements with O(lg(n))
random access, where n is the index of the element you are trying to retreive.
--}
data Arrangement a = Branch a (Arrangement a) (Arrangement a) | Empty
						deriving (Show, Eq, Functor)

left (Branch x l r) = l
left Empty = Empty

right (Branch x l r) = r
right Empty = Empty

value (Branch x l r) = x
value Empty = undefined

unfoldA :: a -> (a -> Arrangement a) -> (a -> Arrangement a) -> Arrangement a
unfoldA seed fl fr = Branch seed (fl seed) (fr seed)

generateA :: (Int -> a) -> Arrangement a
generateA f = subGenerate 1 0
	where
		subGenerate k i = Branch (f i) l r
			where
				l = subGenerate (k * 2) (i + k)
				r = subGenerate (k * 2) (i + 2 * k)

infiniteA :: Arrangement Int
infiniteA = generateA id

cut :: (a -> Bool) -> Arrangement a -> Arrangement a
cut p Empty = Empty
cut p (Branch x l r) = if p x then Branch x (cut p l) (cut p r) else Empty

insert :: a -> Int -> Arrangement a -> Arrangement a
insert x i arr	| i == 0	= Branch x (left arr) (right arr)
				| odd i		= Branch (value arr) (insert x ((i - 1) `div` 2) (left arr)) (right arr)
				| even i	= Branch (value arr) (left arr) (insert x ((i - 2) `div` 2) (right arr))

(?) :: Arrangement a -> Int -> a
(?) arr i	| i == 0	= value arr
			| odd i		= (left arr) ? ((i - 1) `div` 2)
			| even i	= (right arr) ? ((i - 2) `div` 2)

(\\) :: Arrangement a -> (Int, a) -> Arrangement a
(\\) Empty (i, x)				| i == 0	= Branch x Empty Empty
								| otherwise	= insert x i Empty
(\\) (Branch y l r) (i, x)		| i == 0 	= Branch x l r
								| odd i 	= Branch y (l \\ ((i - 1) `div` 2, x)) r
								| even i 	= Branch y l (r \\ ((i - 2) `div` 2, x))

instance Zip Arrangement where
	zip Empty _ = Empty
	zip _ Empty = Empty
	zip (Branch x1 l1 r1) (Branch x2 l2 r2) = Branch (x1, x2) (zip l1 l2) (zip r1 r2)

extract :: Arrangement (Arrangement a -> b) -> (Arrangement a -> Arrangement b)
extract arrf = \arrx -> fmap ($ arrx) arrf

{--

--}

newtype ArrMap a b = ArrMap (Arrangement (a -> b))

instance Category ArrMap where
	id = ArrMap . generateA . const $ id
	(.) (ArrMap arr1) (ArrMap arr2) = ArrMap (zipWith (.) arr1 arr2)

{--
The actual union-find structure. Based on tying the knot. You get path compression for free.
--}

type Size = Int
type ClassName = Int
type Element = (Size, ClassName)
type ReprSet = Arrangement Element
type ReprMap = Arrangement Element
type UF = ReprSet -> ReprMap

-- The initial union-find structure, where everything is a singleton element. Don't have to specify
-- how many there are, it ends up using just however many you need.
singletons :: UF
singletons = \rs -> generateA (\i -> rs ? i)

--- The representative set that should be fed to the UF structure to generate a representative map.
reprSet :: ReprSet
reprSet = generateA $ \i -> (1, i)

-- Finds the representative of a given element in a given union-find structure.
find :: Int -> UF -> Int
find n uf = let rm = uf reprSet in snd $ rm ? n

-- Determines the size (number of element) in the equivalence class of a given element
size :: Int -> UF -> Int
size n uf = let rm = uf reprSet in fst $ rm ? n

-- Returns the union-find structure resulting from the pairing of two equivalence classes.
union :: Int -> Int -> UF -> UF
union i j uf = uf . transducer
	where
		si = size i uf
		ri = find i uf
		sj = size j uf
		rj = find j uf
		transducer rs = rs \\ root \\ complement
			where
				rm = uf . transducer $ rs
				root = if si < sj then (rj, (si + sj, rj)) else (ri, (si + sj, ri))
				complement = if si < sj then (ri, rs ? rj) else (rj, rs ? ri)

-- A union-find structure in which everything is a singleton element, except for m and n, which are
-- paired (in the same equivalence class). Actually, for any UF uf, union i j uf = uf . paired i j,
-- but this is less efficient than the implementation above.
paired :: Int -> Int -> UF
paired i j = \rs -> generateA (\i -> if i == j then rs ? j else rs ? i)
{--paired :: Int -> Int -> UF
paired m n rs = generateA $ \i -> if i == m then (sm + sn, en) else if i == n then (sn, en) else rs ? i
	where
		en = snd $ rs ? n
		sm = fst $ rs ? m
		sn = fst $ rs ? n--}
