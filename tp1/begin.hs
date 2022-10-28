prodList :: Num a => [a] -> a
prodList [] = 1
prodList (x:xs) = x * prodList xs


-- longueur [] = 0
-- longueur (_:xs) = 1 + longueur xs

longueur xs = case xs of
				[]	-> 0
				_:xs->1 + longueur xs
			
