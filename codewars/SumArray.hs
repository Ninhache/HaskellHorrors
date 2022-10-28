-- https://www.codewars.com/kata/576b93db1129fcf2200001e6/train/haskell

findMax :: [Int] -> Int -> Int
findMax [] acc = acc
findMax (x:xs) acc
  | x > acc    = findMax xs x
  | otherwise  = findMax xs acc

removeMax :: [Int] -> Int -> [Int]
removeMax [] acc = []
removeMax (x:xs) acc
  | x == acc  = xs
  | otherwise = x : removeMax xs acc


findMin :: [Int] -> Int -> Int
findMin [] acc = acc
findMin (x:xs) acc
  | x < acc    = findMin xs x
  | otherwise  = findMin xs acc

removeMin :: [Int] -> Int -> [Int]
removeMin [] acc = []
removeMin (x:xs) acc
  | x == acc  = xs
  | otherwise = x : removeMin xs acc

-- sum (removeMin (removeMax array (findMax array (array !! 0))) (findMin array (array !! 0)))