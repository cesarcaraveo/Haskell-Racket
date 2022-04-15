import Data.Maybe

-- filter
myfilter fun [] = []
myfilter fun (x:xs) = -- ex. myfilter (\x -> x /= 0) [0,1,2,3,0,3,5] (if x is not 0)
  if fun x
  then x : (myfilter fun xs)
  else myfilter fun xs

partialSum (x, y, z) = x + z

sum1 xs = 
  case xs of
    [] -> 0
    (x:xs') -> x + (sum1 xs')
    
-- tail recursion    
sum2 xs = 
  let
    f acc list = -- acc keeps track of sum up to the point
      case list of
        [] -> acc
        (x:xs') -> f (x + acc) xs'
  in
    f 0 xs
    
fact1 n =
  if n == 0
  then 1
  else n * fact1 (n-1)

-- tail recursion
fact2 n = 
  let
    aux acc n = 
      if n == 0
      then acc
      else aux (acc * n) (n-1)
  in
    aux 1 n
    
rev1 list =
  case list of
    [] -> []
    (x:xs) -> (rev1 xs) ++ [x]
    
-- tail recursion
rev2 list = 
  let
    aux acc list = 
      case list of
        [] -> acc
        (x:xs) -> aux (x:acc) xs
  in
    aux [] list
    
{-f x =
  let
    y = if true then 5 else 6
  in
    x + y
-}

allGreaterThan42 xs = filter (\x -> x > 42) xs
allGreaterThan xs n = filter (\x -> x > n) xs

allShorterThan1 xs s = filter (\x -> (length x) < (length s)) xs
allShorterThan2 xs s = 
  let
    i = (length s)
  in
    filter (\x -> (length x) < i) xs
    
-- fold
fold' f acc list = 
  case list of
    [] -> acc
    (x:xs) -> fold' f (f acc x) xs
    
numberInRange low hi lst = 
  fold' (\acc el -> acc + (if (el >= low) && (el <= hi) then 1 else 0)) 0 lst
  
allAreShorter s list = 
  let
    i = length s
  in
    fold' (\acc el -> acc && ((length el) < i)) True list
    
compose f g = (\x -> f (g x)) -- . in haskell ex. negate . sum 







