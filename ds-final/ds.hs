import Parser

data JSON = Objet [(String, JSON)]
			| Liste [JSON]
			| Chaine String
            deriving (Show)

toutSauf :: [Char] -> Parser Char
toutSauf xs = do
               ys <- carQuand (`notElem` xs)
               return ys

chaineP :: Parser String
		{-
chaineP = do
           _ <- car '"'
           xs <- many (carQuand (/= '"'))
           _ <- car '"'
           return xs
		-}
chaineP = (\_ xs _ -> xs) <$> car '"' <*> many (carQuand (/= '"')) <*> car '"'

jsonChaine :: Parser JSON
jsonChaine = do
              xs <- chaineP
              return (Chaine xs)



json :: Parser JSON
json = jsonListe <|> jsonObjet <|> jsonChaine

plusVirg :: Parser a -> Parser a
plusVirg p = (do
                 xs <- p
                 _ <- car ','  
                 return xs) <|> p

zeroOuPlusVirg :: Parser a -> Parser [a]
zeroOuPlusVirg p = many (plusVirg p)

unOuPlusVirg :: Parser a -> Parser [a]
unOuPlusVirg p = some (plusVirg p)

jsonListe :: Parser JSON
jsonListe = do
             car '['
             xs <- zeroOuPlusVirg (json)
             car ']'
             return (Liste xs)

clefValeur :: Parser (String, JSON)
clefValeur = do
              clef <- chaineP 
              car ':'
              valeur <- json
              return (clef, valeur)


jsonObjet :: Parser JSON
jsonObjet = do
             car '{'
             cs <- zeroOuPlusVirg (clefValeur)
             car '}'
             return (Objet cs)

