{-# LANGUAGE DeriveFunctor #-}

{--
The Arrangement data type is similar to sequence in that it provides O(lg(n)) random access, but it
is arranged differently. It has the property that an element at a larger index can never be the parent
of an element at a smaller index. This allows us to have infinitely long Arrangements with O(lg(n))
random access, where n is the number of elements in the Arrangement that have been evaluated so far.
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

TODO: Augment with size so that you actually get a(n) performance.
--}

type UF = Arrangement Int -> Arrangement Int -> Arrangement Int

fix f = let x = f x in x

-- The initial union-find structure, where everything is a singleton element. Don't have to specify
-- how many there are, it ends up using just however many you need.
singletons :: UF
singletons = \xs self -> generateA (\i -> xs ? i)

find :: Int -> UF -> Int
find n uf = (fix (uf infiniteA)) ? n

union :: Int -> Int -> UF -> UF
union m n uf = \xs self -> uf (operator (uf xs self)) self
	where
		pm = find m uf
		pn = find n uf
		operator = \xs -> generateA $ \i -> if i == pm then xs ? pn else xs ? i
