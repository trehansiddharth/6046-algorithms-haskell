{-# LANGUAGE DeriveFunctor #-}

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

-- Finds the representative of a given element in a given union-find structure.
find :: Int -> UF -> Int
find n uf = snd $ (uf . generateA $ \i -> (1, i)) ? n

-- Determines the size (number of element) in the equivalence class of a given element
size :: Int -> UF -> Int
size n uf = fst $ (uf . generateA $ \i -> (1, i)) ? n

-- Returns the union-find structure resulting from the pairing of two equivalence classes.
union :: Int -> Int -> UF -> UF
union m n uf = uf . (if sm < sn then paired rm rn else paired rn rm)
	where
		sm = size rm uf
		sn = size rn uf
		rm = find m uf
		rn = find n uf

-- A union-find structure in which everything is a singleton element, except for m and n, which are
-- paired (in the same equivalence class).
paired :: Int -> Int -> UF
paired m n rs = generateA $ \i -> if i == m || i == n then (sm + sn, en) else rs ? i
	where
		en = snd $ rs ? n
		sm = fst $ rs ? m
		sn = fst $ rs ? n
