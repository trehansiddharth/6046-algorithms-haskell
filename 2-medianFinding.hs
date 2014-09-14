data BST a = Empty | Branch Int a (BST a) (BST a)
				deriving (Eq, Show)

size :: BST a -> Int
size Empty = 0
size (Branch s x l r) = s

-- Note: if the list is even-lengthed, it returns the bigger of the two medians
findMedian :: Ord a => [a] -> a
findMedian xs = conquer (1 + length xs `div` 2) . divideByRank $ xs
	where
		divideByRank [] = Empty
		divideByRank xs =  Branch (length xs) (length equal, threshold) (divideByRank lesser) (divideByRank greater)
			where
				threshold = case length xs of
					1 -> xs !! 0
					2 -> xs !! 0
					3 -> xs !! 1
					4 -> xs !! 1
					5 -> xs !! 2
					_ -> findMedian . map findMedian . groupFives $ xs
					where
						groupFives []	= []
						groupFives xs	| length xs < 5	= [xs]
										| otherwise		= (take 5 xs) : groupFives (drop 5 xs)
				lesser = filter (< threshold) xs
				greater = filter (> threshold) xs
				equal = filter (== threshold) xs
		conquer i Empty = error "Cannot find median of an empty set"
		conquer i (Branch s (n, x) l r)	| i > n + size l	= conquer (i - n - size l) r
										| i > size l		= x
										| otherwise			= conquer i l
