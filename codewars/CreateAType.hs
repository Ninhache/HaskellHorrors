data Pile a = PileVide|Maillon a (Pile a) deriving (Eq, Show)

estVide :: Pile a -> Bool
estVide x = x == PileVide
