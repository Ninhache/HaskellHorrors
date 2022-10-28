unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x	= []
			   | otherwise = h x : unfold p h t (t x)

intToBin = unfold (==0) (`mod` 2) (`div` 2)
