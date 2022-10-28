-- alterne::[a]->[a]
-- alterne [] = []
-- alterne (x:xs) = x ++ alterne(tail(xs))
--

-- alterne::[a] -> [a]
alterne [] = []
alterne [x] = [x]
alterne (x:_:xs) = [x] ++ alterne(xs)

combine f [] [] = []
combine f x [] = x
combine f [] x = x
combine f (x1:xs1) (x2:xs2) = f x1 x2 : combine f xs1 xs2

pasPascal::[Integer] -> [Integer]
pasPascal [] = []
pasPascal x = combine (+) (x ++ [0]) ([0] ++ x)

-- pascal :: [[Integer]]
pascal = iterate pasPascal [1]
