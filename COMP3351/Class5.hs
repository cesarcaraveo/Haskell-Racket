f x = case x of 
  1 -> "It's 1"
  2 -> "It's 2"
  _ -> "it's something" -- _ is wildcard
  
f' list = 
  case list of
    [] -> "it's empty"
    (x:xs) -> "something"
    
f'' (_, x, _) = x

len xs = 
  case xs of 
    [] -> 0
    (x:rest) -> 1 + (len rest)
    
len' [] = 0
len' (_:rest) = 1 + (len' rest)

revList [] = []
revList (x:rest) = revList rest ++ [x]

takeN n (x:rest) = 
  if n  <= 0
  then []
  else x : (takeN (n-1) rest)
  
nTimes f n x = -- f is function
  if n == 0
  then x
  else f (nTimes f (n-1) x)
  
double x = x + x

add1 x = x + 1

timesUntilZero f x = 
  if x == 0
  then 0
  else 1 + (timesUntilZero f (f x))
  
addPair (x, y) = x + y

-- takes a function that takes a pair and makes a curried version

 -- anonymous function that takes an x and then anon func that takes a y
mycurry g = \x -> \y  -> g(x, y)

-- 
myuncurry g = \(x, y) -> g x y
  
  
  
  