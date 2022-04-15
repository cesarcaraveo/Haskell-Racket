import Data.List
import Data.Char
import Data.Maybe

-- a1

sDotProduct num (x1, y1) (x2, y2) = ((x1 * x2) + (y1 * y2)) * num

distance (x1, y1) (x2,y2) = sqrt (((x2-x2)^2) + (y2 - y1)^2)

findMax (x:xs) = 
  let
    findMaxHelper currMax (y:ys) = if currMax < y then y else currMax
  in
    findMaxHelper x xs
    
revZip2Lists [] [] = []
revZip2Lists (x:xs) (y:ys) = revZip2Lists xs ys ++ [(x,y)] 

everyThird [] = []
everyThird (x:[]) = []
everyThird (x:y:[]) = []
everyThird (x:y:z:xs) = z : everyThird xs

-- a2

removeAllExcept element [] = []
removeAllExcept element (x:xs) = 
  if element == x
  then x : removeAllExcept element xs
  else removeAllExcept element xs
  
removeAll element [] = []
removeAll element (x:xs) = 
  if element == x
  then removeAll element xs
  else x : removeAll element xs
  
substitute replaceThis withThis [] = []
substitute replaceThis withThis (x:xs) = 
  if x == replaceThis
  then withThis : substitute replaceThis withThis xs
  else x : substitute replaceThis withThis xs

mergeSorted3 x y z = 
  let
    mergeSorted2 hi [] = hi
    mergeSorted2 [] hi = hi
    mergeSorted2 list1@(x:xs) list2@(y:ys) = 
      if x < y
      then x : mergeSorted2 xs list2
      else y : mergeSorted2 list1 ys
  in
    mergeSorted2 (mergeSorted2 x y) z
    
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a) deriving Show
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

nodeValue tree =
  case tree of
    EmptyNode -> error "empty node"
    TriNode a _ _ _  -> a
    
leftChild tree = 
  case tree of
    EmptyNode -> error "empty node"
    TriNode a l m r -> l
    
middleChild tree = 
  case tree of
    EmptyNode -> error "empty"
    TriNode a _ m _ -> m
    
rightChild tree =
  case tree of
    EmptyNode -> error "empty"
    TriNode a _ _ r -> r

inTree element tree = 
  case tree of
    EmptyNode -> False
    TriNode a l m r -> if (nodeValue tree == element)
                                 then True
                                 else
                                   if (inTree element l)
                                   then True
                                   else
                                     if (inTree element m)
                                     then True
                                     else
                                       if (inTree element r)
                                       then True
                                       else False
                                       
leafList tree = 
  case tree of
    EmptyNode -> []
    TriNode a EmptyNode EmptyNode EmptyNode ->  [a]
    TriNode a l m r -> (leafList l) ++ (leafList m) ++ (leafList r)     
    
inOrderMap function tree = 
  case tree of
    EmptyNode -> EmptyNode
    TriNode a l m r -> TriNode (function a) (inOrderMap function l) (inOrderMap function m) (inOrderMap function r)

preOrderFold function acc tree = 
  case tree of
    EmptyNode -> acc
    TriNode a l m r-> 
      let
        newAcc = function acc a
        in
          let
            leftTree = preOrderFold function newAcc l
          in
            let
              middleTree = preOrderFold function leftTree m
            in
              preOrderFold function middleTree r
              
-- a3

onlyLowercase = filter (\x -> if not (null x) then (isLower (head x)) else False)

longestString [] = []
longestString (x:xs) = foldl (\x acc-> if (length x) > (length acc) then x else acc) x xs

longestString' [] = []
longestString' (x:xs) = foldl (\x acc-> if (length x) < (length acc) then x else acc) x xs

longestStringHelper function (x:xs) = 
  foldl (\x acc ->
    if function (length x) (length acc)
    then x
    else acc) x xs
    
longestLowercase [] = ""
longestLowercase list =  (longestString . onlyLowercase) list

revStringRev list =
  let
    uptolow [] = ""
    uptolow (x:xs) = 
      if isLower x
      then x : uptolow xs
      else (toLower x) : uptolow xs
  in
    (reverse . uptolow) list

firstAnswer function [] = Nothing
firstAnswer function (x:xs) = 
  case function x of
    Nothing -> firstAnswer function xs
    Just v -> Just v
    
data Pattern =
  WildcardPat
  | VariablePat (String)
  | UnitPat
  | ConstantPat (Int)
  | ConstructorPat (String, Pattern)
  | TuplePat ([Pattern]) deriving (Show)
  
data Value =
  Constant (Int)
  | Unit
  | Constructor (String, Value)
  | Tuple [Value] deriving (Show)
  
g f1 f2 pat =
 let
   r = g f1 f2
 in
   case pat of
     WildcardPat -> f1 ()
     VariablePat x -> f2 x
     ConstructorPat (_, p) -> r p
     TuplePat values -> foldl (\i p -> (r p) + i) 0 values
     _ -> 0

countWildcards pattern = g (\x -> 1) (\x -> 0)
countWildAndVariableLengths pattern = g (\x -> 1) (\x -> (length x))


    
    
                        
      






