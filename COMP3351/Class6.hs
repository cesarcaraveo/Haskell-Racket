import Data.Maybe

add (x,y) = x + y

-- my curry takes function g and assume g takes a pair of arguments x y
-- want add x -> fun y -> x + y
mycurry g = \x -> \y -> g(x,y)
-- check notes for explination
-- a' = mycurry add
-- b = a' 2 (created half the function -- 2 represents x)
-- call b 4 (2,4)

-- anonymous function has to take pair (x,y)
-- uses pattern matching
myuncurry g = \(x,y) -> g x y
-- ?looks like to uncurry you have to match the thing at the end of the curry  in the first slot

-- same but without pattern matching
myuncurry' g = \f -> g (fst f) (snd f)

add1 x = x + 1

-- adds 1 to each element in the list
add1ToList [] = []
add1ToList (x:rest) = (add1 x) : (add1ToList rest)

-- calls function on each element of list
headsOfList [] = []
headsOfList (x:rest) = (head x) : (headsOfList rest)

funList fun [] = []
funList fun (x:rest) = (fun x) : (funList fun rest)

-- what
collectSubset predicate [] = []
collectSubset predicate (x:rest) = 
  if (predicate x)
  then x:(collectSubset predicate rest)
  else (collectSubset predicate rest)
  
myfold function initialValue [] = initialValue
myfold function initialValue (x:rest) =
  let
    newInitialValue = (function initialValue x)
  in
    myfold function newInitialValue rest
    
data Suit = Club | Diamond | Heart | Spade deriving (Show, Eq)
data Rank = Pip (Int) | Jack | Queen | King | Ace deriving (Eq, Show, Ord)
data Card = Card (Rank) (Suit) deriving (Show, Eq)

-- function to find first pair in hand
findPair [] = Nothing
findPair (x:rest) = 
  if null (filter (==x) rest) 
  then findPair rest
  else Just x
  
-- | means where
heightNames height
  | height < 2.0 = "tiny"
  | height < 5.0 = "short"
  | height < 5.5 = "average"
  | height < 6.2 = "tall"
  | height < 7.0 = "bigly"
  | otherwise = "Andre the Giant"



