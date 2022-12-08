import Parser

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
				deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
			  deriving (Show,Eq)

espacesP :: Parser ()
espacesP = many (car ' ') >>
           pure ()

nomP :: Parser Nom
nomP = some (carQuand (/= ' ')) >>= 
       \cs -> espacesP >>
       pure (cs)

varP :: Parser Expression
varP = do cs <- some(carQuand (' ' /=))
          espacesP
          pure (Var cs)

appliqueHelper :: Expression -> [Expression] -> Expression
appliqueHelper e [] = e
appliqueHelper e (x:xs) = appliqueHelper (App e x) xs


applique :: [Expression] -> Expression
applique [] = undefined
applique [e] = e
applique (e:es) = appliqueHelper e es

exprP :: Parser Expression
exprP = varP  

exprsP :: Parser Expression
exprsP = do
        x <- some exprP
        return (applique x)

exprsP' :: Parser Expression
exprsP' = some exprP >>=
          \x -> return (applique x)

lambdaP :: Parser Expression
lambdaP = do
         car('\\')
         espacesP
         varName <- nomP
         espacesP
         chaine("->")
         espacesP
         reste <- exprsP
         return (Lam varName reste)

