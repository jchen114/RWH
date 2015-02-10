import Data.List
import Data.Ord
  
data List a = Cons a (List a)
              | Nil
                deriving (Show)

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))

data Direction = L | R | S deriving (Show)

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
  | las > lbs = GT
  | las < lbs = LT
  | las == lbs = EQ
  where las = length as
        lbs = length bs

intersperse' :: a -> [[a]] -> [a]
intersperse' a ass
  | length ass > 1 = head ass ++ [a] ++ intersperse' a (tail ass)
  | otherwise = head ass

heightOfTree :: Tree a -> Int
heightOfTree (Node a (Just b) (Just c)) = 1 + max (heightOfTree b) (heightOfTree c)  
heightOfTree (Node a (Nothing) (Nothing)) = 1 

whatTurn :: (Ord a, Num a, Floating a, Fractional a) => (a,a) -> (a,a) -> (a,a) -> Direction
whatTurn (a,b) (c,d) (e,f)
  | slope (a,b) (c,d) < slope (c,d) (e,f) = L
  | slope (a,b) (c,d) > slope (c,d) (e,f) = R
  | otherwise = S
  where slope (a,b) (c,d) = (d-b) / (c-a) 

list2Direction :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [Direction]
list2Direction as
  | length as > 3 = [whatTurn (points as !! 0) (points as !! 1) (points as !! 2)] ++ list2Direction (tail as)
  | otherwise = [whatTurn (as !! 0) (as !! 1) (as !! 2)]
  where points = take 3 

--convexHull :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [(a,a)]

sortAngle :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [(a,a)]
sortAngle as = sortBy (comparing (angle (min as))) $ delete (min as) as
               where angle (a,b) (c,d)
                       | c - a >= 0 = atan ((d-b)/(c-a))
                       | otherwise = pi + atan ((d-b)/(c-a))
                     min = minimumPoint

minimumPoint :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> (a,a)
minimumPoint as = minimumBy minimumPointOrdering as

minimumPointOrdering :: (Ord a, Num a, Floating a, Fractional a) => (a,a) -> (a,a) -> Ordering
minimumPointOrdering (a,b) (c,d)
  | b < d = LT
  | b == d = EQ
  | b > d = GT
