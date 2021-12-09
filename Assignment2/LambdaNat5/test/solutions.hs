member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

removeList :: Int -> [Int] -> [Int]
removeList _ [] = []
removeList x (y:ys) | x == y = ys
                | otherwise = y : removeList x ys

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

prodList :: [Int] -> Int
prodList [] = 1
prodList (x:xs) = x * prodList xs


plus_two :: Int -> Int
plus_two x = x + 2

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
mapList f (x:xs) = f x : mapList f xs


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:(insert x ys)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)


main = do
  print $ (member 1 [2,3,1]);
  print $ (removeList 2 [1,2,3,1,2,3]);
  print $ (sumList [1,2,3,4]);
  print $ (prodList [1,2,3,4]);
  print $ (mapList plus_two [1,2,3,4]);
  print $ (sort [1,2,3,1,2,3]);
