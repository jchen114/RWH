import Data.List

data List a = Cons a (List a)
              | Nil
                deriving (Show)

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))

converse :: List a -> [a]
converse (Cons a as) = a : converse as
converse Nil = []

length' :: [a] -> Int
length' (_:as) = 1 + length' as
length' [] = 0

mean' :: (Integral a, Fractional b) => [a] -> b
mean' as = fromIntegral (sum as) / fromIntegral (length as) 

turnIntoPalindrome :: Integral a => [a] -> [a]
turnIntoPalindrome as = as ++ reverse' as

reverse' :: Integral a => [a] -> [a]
reverse' (a:as) = reverse' as ++ [a]
reverse' [] = []

determineIfPalindrome :: Integral a => [a] -> Bool
determineIfPalindrome as
  | length as == 0 = True
  | head as == last as = True && (determineIfPalindrome . drop 1 . init) as
  | otherwise = False

sortByLengths :: [[a]] -> [[a]]
sortByLengths ass = sortBy order' ass 

order' :: [a] -> [a] -> Ordering
order' as bs
  | length as > length bs = GT
  | length as < length bs = LT
  | length as == length bs = EQ

