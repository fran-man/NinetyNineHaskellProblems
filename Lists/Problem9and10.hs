runLengthCompression :: Eq a => [a] -> [(Int, a)]
runLengthCompression l = map (\x -> (length x, head x)) $ packConsecutive l

packConsecutive :: Eq a => [a] -> [[a]]
packConsecutive [] = []
packConsecutive [x] = [[x]]
packConsecutive l = packConsecutiveLists $ mapElementsToLists l

-- Doesn't really do anything, just makes usage more readable
mapElementsToLists :: [a] -> [[a]]
mapElementsToLists = map (: [])

-- || Pack Consecutive Lists ||
-- If two consecutive lists contain the same elements,
-- we "squash" them together into one list
packConsecutiveLists :: Eq a => [[a]] -> [[a]]
packConsecutiveLists [] = []
packConsecutiveLists (x:xs)
-- If xs is null (empty), then we are done and x contains all relevant elements
    | null xs = [x]
-- If the elements in x are the same as the elements in the first list of xs,
-- then x and head xs need to be combined together and consd onto the rest of xs
    | head x == head (head xs) = packConsecutiveLists ((x ++ head xs) : tail xs)
-- otherwise x is "complete" and the next array in xs has a different element
-- so we recurse
    | otherwise = x : packConsecutiveLists xs