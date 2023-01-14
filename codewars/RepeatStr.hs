repeatStr :: Int -> String -> String
repeatStr 0 _   = ""
repeatStr n str = str ++ repeatStr (n-1) str
