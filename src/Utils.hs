module Utils where


subscript :: [a] -> Int -> Maybe a
subscript [] _ = Nothing
subscript (x:_) 0 = Just x
subscript (_:xs) n = subscript xs (n - 1)
