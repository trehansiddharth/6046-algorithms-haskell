import Data.Array hiding (range)

data VEBTree = Upper { children :: Array Int (Maybe VEBTree), range :: Maybe (Int, Int) } |
				Lower { occupants :: Array Int Bool, range :: Maybe (Int, Int) }
				deriving (Eq, Show)

numberOfChildren :: VEBTree -> Int
numberOfChildren tree = i2 - i1 + 1
	where
		(i1, i2) = case tree of
			Upper c _ -> bounds c
			Lower c _ -> bounds c

capacity :: VEBTree -> Int
capacity = (^2) . numberOfChildren

getMaximum :: VEBTree -> Maybe Int
getMaximum tree = do
	(min, max) <- range tree
	return max

getMinimum :: VEBTree -> Maybe Int
getMinimum tree = do
	(min, max) <- range tree
	return min

ceil :: Float -> Int
ceil x = if fromIntegral f == x then f else f + 1
	where
		f = floor x

fromJust :: Maybe a -> a
fromJust (Just x) = x

buckets :: Int -> Int
buckets = ceil . sqrt . fromIntegral

subindex :: Int -> Int -> (Int, Int)
subindex n x = (i, j)
	where
		x' = x - 1
		i' = floor $ (fromIntegral x') / (fromIntegral n)
		j' = x - (i' * n) - 1
		(i, j) = (i' + 1, j' + 1)

empty :: Int -> VEBTree
empty n = if buckets n == n then lowerTree else upperTree
	where
		upperTree = Upper (listArray (1, buckets n) [Nothing | i <- [1 .. buckets n]]) Nothing
		lowerTree = Lower (listArray (1, buckets n) [False | i <- [1 .. n]]) Nothing

insert :: Int -> VEBTree -> VEBTree
insert x tree = case tree of
	Upper _ _ -> Upper (updatedChildren . children $ tree) updatedRange
	Lower _ _ -> Lower (updatedOccupants . occupants $ tree) updatedRange
	where
		updatedRange = case range tree of
			Nothing -> Just (x, x)
			Just (a, b) -> Just $ if x < a then (x, b) else if x > b then (a, x) else (a, b)
		updatedChildren arr = arr // [(i, Just $ insert j xTree)]
			where
				xTree = case arr ! i of
					Nothing -> empty n
					Just t -> t
				n = numberOfChildren tree
				(i, j) = subindex n x
		updatedOccupants arr = arr // [(x, True)]

delete :: Int -> VEBTree -> VEBTree
delete x tree = case tree of
	Upper _ _ -> Upper updatedChildren updatedRangeUpper
	Lower _ _ -> Lower updatedOccupants updatedRangeLower
	where
		updatedRangeUpper = if a == b then Nothing else Just (findMin a updatedChildren, findMax b updatedChildren)
			where
				a = fromJust $ getMinimum tree
				b = fromJust $ getMaximum tree
				findMin i arr = case arr ! i of
					Nothing -> findMin (i + 1) arr
					Just t -> fromJust $ getMinimum t
				findMax i arr = case arr ! i of
					Nothing -> findMax (i - 1) arr
					Just t -> fromJust $ getMaximum t
		updatedRangeLower = if a == b then Nothing else Just (findMin a updatedOccupants, findMax b updatedOccupants)
			where
				a = fromJust $ getMinimum tree
				b = fromJust $ getMaximum tree
				findMin i arr = case arr ! i of
					False -> findMin (i + 1) arr
					True -> i
				findMax i arr = case arr ! i of
					False -> findMax (i - 1) arr
					True -> i
		updatedChildren = arr // [(i, newXTree)]
			where
				newXTree = if getMinimum xTree == getMaximum xTree then Nothing else Just $ delete j xTree
				xTree = fromJust $ arr ! i
				n = numberOfChildren tree
				(i, j) = subindex n x
				arr = children tree
		updatedOccupants = arr // [(x, False)]
			where
				arr = occupants tree
