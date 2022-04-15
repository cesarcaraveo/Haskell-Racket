import Data.Maybe

-- map
funList fun [] = []
funList fun (x:xs) = (fun x) : (funList fun xs)

add1 x = x + 1

-- filter
myfilter fun [] = []
myfilter fun (x:xs) = 
  if fun x
  then x:(myfilter fun xs)
  else (myfilter fun xs)
  
isEven v = (v `mod` 2) == 0
allEvens = myfilter isEven

data Exp = Constant (Integer) | Negate (Exp) | Add (Exp) (Exp) | Mult (Exp) (Exp) deriving (Show, Eq)

trueOfAllConstants fun e =
  case e of
    Constant i -> fun i
    Negate e1 -> trueOfAllConstants fun e1
    Add e1 e2 -> (trueOfAllConstants fun e1) && (trueOfAllConstants fun e2)
    Mult e1 e2 -> (trueOfAllConstants fun e1) && (trueOfAllConstants fun e2)
    
allEven = trueOfAllConstants isEven

eval e = 
  case e of
    Constant i -> i
    Negate i -> error "can't negate!"
    Add e1 e2 -> (eval e1) + (eval e2)
    Mult e1 e2 -> (eval e1) + (eval e2)

x = 1
f y =  -- ex. g = f 4
  let
    x = y + 1 -- 5 = 4 + 1
  in
    (\z -> x + y + z) -- 5 + 4 + z; need z = g 6 which equals 5 + 4 + 6 = 15
    
f' g =
  let
    x = 3
  in
    g 2

f'' y =
  let
    q = y + 1
  in
    (\z -> q + y + z)
    



