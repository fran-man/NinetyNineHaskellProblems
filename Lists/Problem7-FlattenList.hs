data MyNestedList a = Elem a | List [MyNestedList a]

-- || Flatten a Nested List ||
-- Two different cases as each element may be a "plain" element,
-- Or a list (which may be further nested)
-- Note the easier way would be to use the standard "concat" function in place of "myFlatten",
-- but I kept it like this for learning purposes and to be explicit
flattenMyNestedList :: MyNestedList a -> [a]
flattenMyNestedList (Elem a) = [a]
flattenMyNestedList (List l) = flattenMyNestedList (head l) ++ myFlatten (map flattenMyNestedList (tail l))

-- Turn a list of lists into a list
-- This is easy here, since we know all elements are a list
-- So this can be easily done using foldr
myFlatten :: [[a]] -> [a]
myFlatten = foldr (++) []