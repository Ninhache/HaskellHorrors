infA1 :: a -> [a] 
infA1 x = x : infA1 x

infA2 :: a -> [a]
infA2 x = iterate (\y -> y) x

randomFunction :: (a -> b) -> (b -> c) -> a -> c
randomFunction f1 f2 x = f2 (f1 x)

data Arborescence a = Empty | File a | Folder a [Arborescence a]

countFiles :: Arborescence a -> Int
countFiles Empty = 0
countFiles (File a) = 1
countFiles (Folder a []) = 0
countFiles (Folder a (x:xs)) = countFiles x + countFiles (Folder a xs)


------

primes :: [Int]
primes = eratosthene [2..]

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n xs = filter (\x -> x `mod` n /= 0) xs

eratosthene :: [Int] -> [Int]
eratosthene [] = []
eratosthene (x:xs) = x : eratosthene (removeMultiples x xs)

-- take 100 primes

type File a = ([a], [a])

fileVide :: File a
fileVide = ([],[])

estFileVide :: File a -> Bool
estFileVide ([], []) = True
estFileVide _ = False

enfile :: File a -> a -> File a
enfile (xs, ys) x = (xs, x : ys)











