data SingleOrMultiple a = Single a | Multiple Int a deriving Show

directEncoding :: Eq a => [a] -> [SingleOrMultiple a]
directEncoding [] = []
directEncoding [x] = [Single x]
directEncoding l = compressSomList $ map Single l

-- || Compress a list of SingleOrMultiple ||
-- Take a list of SingleOrMultiple values and compress them into a smaller list
-- If two adjacent SingleOrMultiple have the same value then they will be joined and the count increased
-- to represent the sum of the original two SingleOrMultiple
-- ----
-- Example:
--     compressSomList [Single 'foo', Single 'foo'] returns [Multiple 2 'foo']
--     compressSomList [Multiple 1 'foo', Multiple 2 'foo'] returns [Multiple 3 'foo']
-- ----
-- If the value of the first and second items are the same, combine them into a new multiple
-- (Multiple (somCount x + somCount (head xs)) (somVal x)), add this to the rest of the list (tail xs)
-- and then recurse on this whole list
-- Otherwise, x is complete, so we simply recurse on the rest of the list xs
compressSomList :: Eq a => [SingleOrMultiple a] -> [SingleOrMultiple a]
compressSomList [] = []
compressSomList [x] = [x]
compressSomList (x:xs)
    | somValueEquals x (head xs) = compressSomList 
        (Multiple (somCount x + somCount (head xs)) (somVal x) : tail xs)
    | otherwise = x : compressSomList xs

somCount :: SingleOrMultiple a -> Int
somCount (Single _) = 1
somCount (Multiple x _) = x

somVal :: SingleOrMultiple a -> a
somVal (Single x) = x
somVal (Multiple _ x) = x

-- Extract the values and compare
-- This is NOT the same as equality of the SingleOrMultiple itself,
-- for example somValueEquals (Single 'a') (Multiple 5 'a') returns True
somValueEquals :: Eq a => SingleOrMultiple a -> SingleOrMultiple a -> Bool
somValueEquals s1 s2 = somVal s1 == somVal s2