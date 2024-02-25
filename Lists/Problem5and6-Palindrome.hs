-- || Problem 5: Reverse a list ||
-- If list is empty, return empty list
-- Otherwise, take tail of list, reverse it,
-- and add the first element onto the end
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- || Problem 6: Palindrome test ||
-- Use solution of problem 5 to make this easy!
-- Eq a => is required to allow the == comparison to work
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l
    | myReverse l == l = True
    | otherwise = False