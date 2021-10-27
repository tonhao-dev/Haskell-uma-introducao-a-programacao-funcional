quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
    quickSort minors ++ [x] ++ quickSort majors
    where
        minors = filter (<=x) xs
        majors = filter (>x) xs