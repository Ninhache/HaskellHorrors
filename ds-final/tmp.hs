import Parser

tmp = many (car ',') >>= \x -> return x
