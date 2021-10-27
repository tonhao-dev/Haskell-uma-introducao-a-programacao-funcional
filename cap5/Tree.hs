data Tree a = Null | Leaf a | Node (Tree a) a (Tree a) deriving Show

-- Árvore usada como exemplo: https://bityli.com/lEuC3m
tree :: Tree Int
leftTree = 
    Node 
        (Node 
            (Leaf 9) 
            12 
            (Leaf 14)
        ) 
        17 
        (Node 
            (Leaf 19) 
            23 
            (Null)
        )
rightTree = 
    Node 
    (Node 
        (Null) 
        54 
        (Leaf 67)
    ) 
    72 
    (Leaf 76)
tree = Node leftTree 50 rightTree

search :: (Eq a, Ord a) => Tree a -> a -> Bool
search Null _ = False
search (Leaf value) searched = value == searched
search (Node leftTree value rightTree) searched 
    | value == searched = True
    | searched < value  = search leftTree searched
    | otherwise         = search rightTree searched

preOrder :: Tree a -> [a]
preOrder Null = []
preOrder (Leaf value) = [value]
preOrder (Node leftTree value rightTree) =
    [value] ++ preOrder leftTree ++ preOrder rightTree

inOrder :: Tree a -> [a]
inOrder Null = []
inOrder (Leaf value) = [value]
inOrder (Node leftTree value rightTree) = 
    inOrder leftTree ++ [value] ++ inOrder rightTree

postOrder :: Tree a -> [a]
postOrder Null = []
postOrder (Leaf value) = [value]
postOrder (Node leftTree value rightTree) =
    postOrder leftTree ++ postOrder rightTree ++ [value]