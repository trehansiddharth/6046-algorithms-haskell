import Data.Array
import Data.List (groupBy, maximumBy)
import Data.Ord (comparing)

main = return ()

data Event = Event { name :: String, startTime :: Int, endTime :: Int, weight :: Int }
				deriving (Eq, Show)

type EventListing = [Event]

type Schedule = Array Int [Event]

fix f = let x = f x in x

findOptimalEventListing :: Schedule -> EventListing
findOptimalEventListing schedule = snd $ optimums ! minStart
	where
		optimums = fix $ \arr -> listArray (minStart, maxEnd) [optimalFor t arr | t <- [minStart .. maxEnd]]
		(minStart, maxEnd) = bounds schedule
		optimalFor t arr = if t == maxEnd then (0, []) else case schedule ! t of
			[] -> arr ! (t + 1)
			events -> maximumBy (comparing fst) . map processEvent $ events
			where
				processEvent event = (getEventWeight event, getEventListing event)
				getEventWeight event = (weight event) + (fst $ arr ! (endTime event))
				getEventListing event = event : (snd $ arr ! (endTime event))

makeSchedule :: EventListing -> Schedule
makeSchedule eventListing = initialArray // collect startTime eventListing
	where
		initialArray = listArray (minStart, maxEnd) [[] | i <- [minStart .. maxEnd]]
		minStart = minimum . map startTime $ eventListing
		maxEnd = maximum . map endTime $ eventListing

collect :: (Eq a, Eq b) => (a -> b) -> [a] -> [(b, [a])]
collect f = map (\xs -> (f . head $ xs, xs)) . groupBy (\x y -> f x == f y)


