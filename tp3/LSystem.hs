import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]


motSuivant :: Regles -> Mot -> Mot
motSuivant f (x:xs) = f x ++ motSuivant f xs

lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a f = lsysteme (motSuivant f a) f

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
type EtatDessin = (EtatTortue, Path)
getX :: Point -> Float
getX (x,_) = x

getY :: Point -> Float
getY (_,y) = y

etatInitial :: Config -> EtatTortue
etatInitial (x,_,_,_,_) = x

longueurPas :: Config -> Float
longueurPas (_,x,_,_,_) = x

facteurEchelle :: Config -> Float
facteurEchelle (_,_,x,_,_) = x

angle :: Config -> Float
angle (_,_,_,x,_) = x

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,x) = x

avance :: Config -> EtatTortue -> EtatTortue
avance c ((x,y), angle) = ((x + longueurPas c * (cos angle) , y + longueurPas c * sin angle), angle)


tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche c (point, angle) = (point, angle * cos(90))

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite c (point, angle) = (point, angle * cos(-90))

filtreSymboleTortue :: Config -> Mot -> Mot
-- filtreSymboleTortue c (m:ms) = filter (\x 
filtreSymboleTortue config mot = filter (\x -> elem x (symbolesTortue config)) mot

-- EtatDessin = (EtatTortue, Path)
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (tortue, path) symbol = (tortue, path ++ [point])
	where (point, _) = (symboleHandler symbol) config tortue


symboleHandler :: Symbole -> (Config -> EtatTortue -> EtatTortue)
symboleHandler 'F' = avance
