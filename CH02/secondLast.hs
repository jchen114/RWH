secondToLast :: [a] -> Maybe a
secondToLast xs
  | length xs > 1 = Just $ last $ init xs
  | otherwise = Nothing
