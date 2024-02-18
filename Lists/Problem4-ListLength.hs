myLen :: [a] -> Int
myLen l = myLenCounter l 0

myLenCounter :: [a] -> Int -> Int
myLenCounter [] n = n
myLenCounter (x:xs) n = myLenCounter xs (n+1)