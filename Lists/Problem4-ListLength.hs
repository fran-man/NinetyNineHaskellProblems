-- || Function to find the length of a list ||
--
-- This function takes a list 'l' and uses helper function 'myLenCounter'
-- to count its elements.
myLen :: [a] -> Int
myLen l = myLenCounter l 0

-- || Helper function to count the length of a list ||
--
-- This function recursively traverses the list, counting each element until
-- it reaches the end. The second argument 'n' is an accumulator
-- This function is needed to keep the signature of myLen clean
-- so the caller does not need to think about the accumulator
myLenCounter :: [a] -> Int -> Int
myLenCounter [] n = n
myLenCounter (x:xs) n = myLenCounter xs (n+1)