-- :set -package QuickCheck
-- import Test.QuickCheck

data Tree color value = Leaf | Node color value (Tree color value) (Tree color value) deriving (Show)

mapTree :: (color -> color) -> (value -> value) -> Tree color value -> Tree color value
mapTree _ _ Leaf = Leaf
mapTree fcolor fvalue (Node color value left right) = (Node (fcolor color) (fvalue value) (mapTree fcolor fvalue right) (mapTree fcolor fvalue left))

size :: Tree color value -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

height :: Tree color value -> Int
height Leaf = 0
height (Node _ _ left right) = 1 + max (height left) (height right)


dimension :: (a -> a -> a) -> a -> Tree color value -> a
dimension _ v Leaf = v
dimension f v (Node _ _ left right) = f (dimension f v left) (dimension f v right)

leftComb :: [(color, value)] -> Tree color value
leftComb [] = Leaf
leftComb ((color, value):xs) = (Node color value (leftComb xs) Leaf)

-- Use with QuickCheck
-- prop_Heigh xs = length xs == height (leftComb xs)

isPerfect :: Tree color value -> Bool
isPerfect Leaf = True
isPerfect (Node _ _ left right) = height left == height right && isPerfect left && isPerfect right

generatePerfectTree :: Int -> [(color, value)] -> Tree color value
generatePerfectTree n xs = tree where
              (tree, _) = generatePerfectTreeInt n xs

generatePerfectTreeInt :: Int -> [(color, value)] -> (Tree color value, [(color, value)])
generatePerfectTreeInt 1 ((c, v) : xs) = ((Node c v Leaf Leaf), xs)
generatePerfectTreeInt n xs = ((Node c0 v0 left right), ps) where
              (left, ys) = generatePerfectTreeInt (n - 1) xs
              (c0, v0) = head ys
              (right, ps) = generatePerfectTreeInt (n - 1) (tail ys)

infiniteArray :: a -> [a]
infiniteArray x = xs
     where xs = x : xs

infiniteArrayOfLetters :: [Char]
infiniteArrayOfLetters = ['a' ..]

infiniteNodeOfLetters :: [Char] -> [((), Char)]
infiniteNodeOfLetters (x : xs) = [((), x)] ++ infiniteNodeOfLetters xs

flatten :: Tree color value -> [(color, value)]
flatten Leaf = []
flatten (Tree c v) = [(c,v)]
flatten (Node c v left right) = flatten left : [(c,v)] : flatten right



-- flatten (Node c v (Tree cL vL) (Tree cR vR)) = [(cL, vL)] : flatten : [(cR, vR)]








