lengthInt :: (Show n, Integral n) => n -> Int
lengthInt n = length $ show n

intToArray n = map (read . pure :: Char -> Int) (show n)

narcissistic n = sum (map (\x -> x ** round (lengthInt n)) (intToArray n)) == (lengthInt n)
