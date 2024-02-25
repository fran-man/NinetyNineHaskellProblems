-- || Flatten a Nested List ||
data MyNestedList a = Elem a | List [MyNestedList a]

flattenMyNestedList :: MyNestedList a -> [a]
flattenMyNestedList (Elem a) = [a]
flattenMyNestedList (List l) = flattenMyNestedList (head l) ++ myFlatten (map flattenMyNestedList (tail l))

myFlatten :: [[a]] -> [a]
myFlatten [] = []
myFlatten (x:xs) = x ++ myFlatten xs