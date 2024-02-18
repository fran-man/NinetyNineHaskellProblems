lastElemButOne :: [a] -> Maybe a
lastElemButOne [] = Nothing
lastElemButOne (x:xs)
    | null xs = Just x
    | otherwise = lastElemButOne xs