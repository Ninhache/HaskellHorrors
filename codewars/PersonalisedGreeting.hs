
greet :: String -> String -> String
greet name owner = if name == owner
					  then "Hello boss"
					else "Hello guest"
