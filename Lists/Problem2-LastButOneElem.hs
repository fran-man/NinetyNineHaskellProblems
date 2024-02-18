lastElemButOne :: [a] -> Maybe a
lastElemButOne [] = Nothing
lastElemButOne (x:xs)
    | length xs == 1 = Just x
    | otherwise = lastElemButOne xs