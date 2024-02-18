nthElement :: [a] -> Int -> Maybe a
nthElement [] _ = Nothing
nthElement (x:xs) n
    | n > length xs + 1 = Nothing
    | n == 1 = Just x
    | otherwise = nthElement xs (n-1)

nthElementOfAlpha = nthElement ['a'..'z']