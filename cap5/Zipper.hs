data Zipper a = Z [a] [a] deriving Show

toZipper :: [a] -> Zipper a
toZipper xs = Z [] xs

fromZipper :: Zipper a -> [a]
fromZipper (Z ls rs) = reverse ls ++ rs

focus :: Zipper a -> a
focus (Z _ []) = error "empty list"
focus (Z _ (x:xs)) = x

walkLeft, walkRight :: Zipper a -> Zipper a
walkLeft  (Z [] rs) = Z [] rs
walkLeft  (Z (x:ls) rs) = Z ls (x:rs)

walkRight (Z ls []) = Z ls []
walkRight (Z ls (x:rs)) = Z (x:ls) rs

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

data ZipTree a = ZT { tfocus :: Tree a, 
                      history :: Either (Tree a) (Tree a)
                    } deriving Show