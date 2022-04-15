demo x = 
  if x < 3
  then "yay!"
  else "boo!"
  
demo' x =
  case x < 3 of
    True -> "yay!"
    False -> "boo!"
    
data Height = Tall | Short deriving (Show)

headOf [] = error "empty list"
headOf (x:xs) = x

dropLast [] = error "empty"
dropLast list@(x:xs) = 
  if (length list) == 1
  then []
  else x : (dropLast xs)
  
data Person = Person {firstname :: String, lastname :: String} deriving (Eq, Show)

dropFirst [] = error "empty"
dropFirst (x:xs) = xs

elemOf val [] = False
elemOf val (x:xs) =
  if (val == x)
  then True
  else elemOf val xs
  

  