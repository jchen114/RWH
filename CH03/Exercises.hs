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
  | calcDir (a,b) (c,d) (e,f) < 0 = R
  | calcDir (a,b) (c,d) (e,f) > 0 = L
  | otherwise = S
  where calcDir (a,b) (c,d) (e,f) = (c-a)*(f-b) - (d-b)*(e-a) 

list2Direction :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [Direction]
list2Direction as
  | length as > 3 = [whatTurn (points as !! 0) (points as !! 1) (points as !! 2)] ++ list2Direction (tail as)
  | otherwise = [whatTurn (as !! 0) (as !! 1) (as !! 2)]
  where points = take 3 

--let a = [(3,2),(2,1),(5,3),(3,4),(4,1),(1,3),(3,3),(4,5),(3,0),(5,2),(2,2),(4,3)]

convexHull :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [Direction]
convexHull as = list2Direction $ (sortAngle . removeRedXCoord) as ++ [min as]
                where min as = minimumPoint as

removeRedXCoord :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [(a,a)]
removeRedXCoord as = concat $ map minMax $ (groupY . sortX) as
  where sortX = sortBy (comparing (\(x,y) -> x))
        groupY = groupBy (\(x,y) -> \(u,v) -> x == u)
        minMax as
          | length as > 2 = [head (f as)] ++ [last (f as)]
          | otherwise = as
          where f as = sortBy (comparing (\(x,y) -> y)) as
   
sortAngle :: (Ord a, Num a, Floating a, Fractional a) => [(a,a)] -> [(a,a)]
sortAngle as = sortBy (comparing (angle (min as))) as
               where angle (a,b) (c,d)
                       | a == c && b == d = 0
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
