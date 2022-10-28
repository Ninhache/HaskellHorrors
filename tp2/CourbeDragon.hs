import Graphics.Gloss
--type Point = (Float, Float)
--type Path = [Point]

pointAIntercaler :: Point -> Point -> Point 
pointAIntercaler (xa,ya) (xb, yb) = (((xa + xb) / 2 + (yb - ya)/2) , ((ya + yb) / 2 + (xa - xb) / 2))

--pasDragon :: Path -> Path
--pasDragon [x] = [x]
--pasDragon (x:y:xs) = x : pointAIntercaler x y : pasDragon2 (y : xs)


--pasDragon2 :: Path -> Path
--pasDragon2 [x] = [x]
--pasDragon2 (x:y:xs) = x : pointAIntercaler y x : pasDragon (y : xs)

pasDragon [] = []
pasDragon [x] = [x]
pasDragon [x,y] = x : pointAIntercaler x y : [y]
pasDragon (x:y:z:tail) = x : pointAIntercaler x y : y : pointAIntercaler z y : pasDragon (z:tail)


-- dragon = iterate pasDragon [[1,1] [1,2]]

main = animate (InWindow "Dragon" (500,500) (0,0)) white (dragonAnime (50,250) (450, 250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 40))

dragon a b = iterate pasDragon [a,b]
