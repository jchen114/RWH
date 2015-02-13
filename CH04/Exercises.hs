
safeHead' :: [a] -> Maybe a
safeHead' [] = Nothing
safeHead' (a:as) = Just a

safeTail' :: [a] -> Maybe [a]
safeTail' [] = Nothing
safeTail' as = Just $ tail as

safeLast' :: [a] -> Maybe a
safeLast' [] = Nothing
safeLast' as = Just $ last as

safeInit' :: [a] -> Maybe [a]
safeInit' [] = Nothing
safeInit' as = Just $ init as

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith f as = front : splitWith f (dropWhile f back)
  where (front, back) = break f as 

--transpose :: String -> String
--transpose str = lines str
