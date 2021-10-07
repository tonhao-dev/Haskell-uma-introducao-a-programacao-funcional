data Tree a = Null | Leaf a | Node (Tree a) a (Tree a) deriving Show

tree :: Tree Int
leftTree = Node (Node (Null) 9 (Node (Leaf 12) 14 (Null))) 17 (Node (Leaf 19) 23 (Null))
rightTree = Node (Node (Null) 54 (Node (Leaf 67) 72 (Null))) 76 (Null)
tree = Node leftTree 50 rightTree

search :: (Eq a, Ord a) => Tree a -> a -> Bool
search Null _ = False
search (Leaf value) searched = value == searched
search (Node leftTree value rightTree) searched 
    | value == searched = True
    | searched < value  = search leftTree searched
    | otherwise         = search rightTree searched
