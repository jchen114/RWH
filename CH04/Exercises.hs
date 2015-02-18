import Data.List
import Data.Char

type ErrorMessage = String
  
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

transpose' :: String -> String
transpose' str = (intercalate "\n" . transpose . lines) str

asInt_fold :: String -> Int
asInt_fold str
  | head str == '-' = (-1) * foldr digify 0 (reverse $ tail str)
  | otherwise = foldr digify 0 (reverse str)
  where digify x a = a*10 + digitToInt x

asInt_either :: String -> Either ErrorMessage Int
asInt_either xs
  | (head xs) `notElem` validChars = Left ("non-digit '" ++ [head xs] ++ "'" )
  | otherwise = Right (asInt_fold xs)
  where validChars = ['0'..'9'] ++ ['-']

concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (a:as)
  | f a = [a] ++ takeWhile' f as
  | otherwise = []
                
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr h [] xs
  where h x y
          | f x = [x] ++ y
          | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = [[]]
groupBy' f (x:xs) = foldl h [[x]] xs
  where h x y
          | f ((head . last) x) y = init x ++ [last x ++ [y]]
          | otherwise = x ++ [[y]]
        
