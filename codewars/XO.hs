import GHC.Unicode

count :: Char -> [Char] -> Int
count x =  length . filter (==(toLower x))

xo :: String -> Bool
xo str = (count 'x' str) == (count 'o' str)

