-- sommeDeXY::(Int, Int) -> Int
-- sommeDeXY(x,y) = x + y

sommeDeXY::Int -> Int -> Int
sommeDeXY x = (\y -> x + y)


somme::Num a => [a] -> a
somme [] = 0
somme (x:xs) = x + somme xs

longueur xs = case xs of
		[] -> 0
		_:xs->1 + longueur xs


-- last : donne le dernier element d'un tableau
-- init : donne le premier element du'n tableau
-- last2 => [a] -> a
last2 (x:xs) = case xs of
				[] -> x
				otherwise -> last2 xs

init2 xs = reverse(tail(reverse xs))

tabAt2 (c, a) = case a of
				 	0 -> head c -- result
					otherwise -> tabAt2 (tail(c),(a-1))
