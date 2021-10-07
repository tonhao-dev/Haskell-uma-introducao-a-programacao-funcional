maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv x y = Just (x `div` y) 


eitherDiv :: Int -> Int -> Either String Int
eitherDiv _ 0 = Left "Division failed"
eitherDiv x y = Right (x `div` y)