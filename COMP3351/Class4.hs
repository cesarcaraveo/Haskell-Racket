--minMax [] = error "no min max of an empty list"
--minMax (x:[]) = (x, x) -- if only one element
--minMax (x:y:rest) =  -- at least two elements
  --let -- base case, nothing left,
   -- minMaxHelper [] curMin curMax = (curMin, curMax)
    
    -- just one element left
    --minMaxHelper (x:[]) curMin curMax =  -- takes list and current min and current max
      --if x < curMin -- if x is new min
      --then (x, curMax) -- update min
      --else if x > curMax -- if x is new max
        --then (curMin, x) -- update max
       -- else (curMin, curMax) -- if x is nothing, return current min and max
        
      -- list has at least two elements
     --minMaxHelper (x:y:rest) curMin curMax = 
       -- let secondAndThirdComparisons smallest biggest curMin curMax = 
         -- let newmin = if smallest < curMin 
           -- then smallest
            --else curMin
            --newmax = if biggest > curMax
             -- then biggest
             -- else curMax
          --in
           -- minMaxHelper rest newmin newmax
      --  in
        -- do first comparison
          --if x < y
         -- then secondAndThirdComparisons x y curMin curMax
          --else secondAndThirdComparisons y x curMin curMax
  --in
   -- if x < y
    --then minMaxHelper rest x y
   -- else minMaxHelper rest y x
   
--x = True
--case x of 
 -- True -> "it's true!"
 -- False -> "it's false!"
  
myMap fun [] = []
myMap fun (x:xs) = (fun x) : (myMap fun xs)

-- equivalent

myMap' fun lst = 
  case lst of
    [] -> []
    (x:xs) -> (fun x) : (myMap' fun xs)
    
dotProduct p1 p2 =
  let
    (x,  y) = p1
    (x', y') = p2
  in
    x * x' + y * y'
    
-- haskell's version of a struct
-- Pos makes types of Position (Pos is the constructor)
-- deriving: inhereting (Eq = can use equal sign, Show = can print (toString), Ord = comparisons)
data Position = Pos { x :: Int, y :: Int } deriving (Eq, Show, Ord)

-- creating
--p = Pos { x = 5, y = 6 }
--p' = Pos 5 6
-- x p (acessing x in p) -- try to avoid

-- better way of acessing
-- case p of
--  Pos x y -> x + y

addPos (Pos {x = x', y = y'}) (Pos {x = x'', y = y''}) = Pos (x' + x'') (y' + y'') -- avoid
addPos' (Pos x y) (Pos x' y') = Pos (x + x') (y + y') -- better

type Velocity = (Int, Int) -- Velocity refers to (Int, Int) tuple

data IntList = Nil | Cons (Int) (IntList) deriving (Eq, Show)

append' xlist ylist = 
  case xlist of -- examine xlist
    Nil -> ylist -- if type Nil (empty), return ylist
    Cons x xs -> Cons x (append' xs ylist) -- if type Cons, create Cons 
    
-- same thing as above
--append'' [] ylist = ylist
--append'' (Cons x xs) ylist = Cons x (append'' xs ylist)

-- generics
data Option a = None | Some a deriving (Eq, Show)

data MyList a = MyNil | MyCons a (MyList a) deriving (Eq, Show)

-- use cases instead of head
first list = (head list)
first' list = 
  case list of
    [] -> error "empty"
    (x:xs) -> x
    
second list = (head (tail list))
second' list = 
  case list of 
    [] -> error "empty list"
    (x:[]) -> error "only one element"
    (x:y:rest) -> y
    
tupleDistance [] [] = 0 -- base case
tupleDistance xlist ylist = 
  let
    tupleSumSquared [] [] = 0 -- empty (put Nothing here)
    tupleSumSquared (x:xs) (y:ys) = (y-x)^2 + (tupleSumSquared xs ys)
  in
    sqrt (tupleSumSquared xlist ylist)
    
tupleDistance' xlist ylist = 
  let
    sum = case (xlist, ylist) of 
      ([], []) -> 0
      (x:xs, y:ys) -> (y-x)^2 + (tupleDistance' xs ys)
  in
    sqrt sum


            