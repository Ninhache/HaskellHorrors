-- Recursif
duplique1 :: [a] -> [(a,a)]
duplique1 [] = []
duplique1 (x:xs) = [(x,x)] ++ duplique2 xs

-- Fonction standard
duplique2 :: [a] -> [(a,a)]
duplique2 xs = map (\x -> (x,x)) xs

-- Liste de compréhension
duplique3 :: [a] -> [(a,a)]
duplique3 xs = [(x,x) | x <- xs]

-- [1,2,3] = [(1,2), (2,3)]
exo1q31 :: [a] -> [(a,a)]
exo1q31 [] = []
exo1q31 [a] = []
exo1q31 (x:xs) = [(x, (head xs))] ++ exo1q31 xs

-- coupleDistinct :: [a] -> [(a,a)]

