compressList :: Eq a => [a] -> [a]
compressList [] = []
compressList (x:xs)
    | null xs = [x]
    | x == head xs = compressList xs
    | otherwise = x : compressList xs