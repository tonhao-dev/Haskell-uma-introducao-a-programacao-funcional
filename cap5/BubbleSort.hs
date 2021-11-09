-- Going from left to right, swaps two adjacent elements if they are not in order.
-- After the first go, the largest element in the list has bubbled up to the end
-- of the list. In the next go, we start swapping from the first element to the
-- penultimate element and so forth.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = go xs (length xs -1)
  where go xs limit | limit > 0 = let swapped = swapTill xs limit in
                                  go swapped (limit -1)
                    | otherwise = xs

-- Swaps adjacent elements in a list if they are not in order, until a limit.
-- After this, the largest elements, from limit to (length xs),
-- are sorted at the list's end.
swapTill :: (Ord a, Num p) => [a] -> p -> [a]
swapTill xs limit = go xs 0
  where go xs count | count < limit = swap xs
                    | otherwise = xs
                      where swap [x] = [x]
                            swap (x:y:xs) | x < y     = x : (go (y:xs) (count +1))
                                          | otherwise = y : (go (x:xs) (count +1))
