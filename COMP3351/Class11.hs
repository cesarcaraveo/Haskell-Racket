module Class11 where
import Data.Char
import Data.Maybe

-- assignment 3 solution
onlyLowercase = filter (\x -> if not (null x) then (isLower (head x)) else False)

longestString [] = ""
longestString (x:xs) = foldl (\acc str -> if (length str) > (length acc) then str else acc) x xs

longestString' [] = ""
longestString' (x:xs) = foldl (\acc str -> if (length str) >= (length acc) then str else acc) x xs

longestStringHelper function [] = []
longestStringHelper function (x:xs) = foldl (\acc str -> if function (length str) (length acc) then str else acc) x xs

longestString3 = longestStringHelper (>)

longestString4 = longestStringHelper (>=)

longestLowercase = (longestString . onlyLowercase) -- partial application (need the list)

revStringRev = (map toLower) . reverse

firstAnswer function [] = Nothing
firstAnswer function (x:xs) = 
  let
    result = (function x)
  in
    if result == Nothing
    then firstAnswer function xs
    else result
    
firstAnswer' function [] = Nothing
firstAnswer' function (x:xs) = 
  case function x of 
    Nothing -> firstAnswer' function xs
    foo@(Just y) -> foo
    --Just y -> Just y
    
allAnswers fun lst = 
  let
    helper acc [] = Just acc
    helper acc (x:xs) = 
      case fun x of
        Nothing -> Nothing
        Just lst' -> helper (acc ++ lst') xs
  in
    case lst of
      [] -> Just []
      _ -> helper [] lst
      
data Pattern =
  WildcardPat
  | VariablePat (String)
  | UnitPat | ConstantPat (Int)
  | ConstructorPat (String, Pattern)
  | TuplePat ([Pattern]) deriving (Eq, Show)

data Value =
  Constant (Int)
  | Unit
  | Constructor (String, Value)
  | Tuple [Value] deriving (Eq, Show)
  
g f1 f2 pat = 
  let
    r = g f1 f2
  in
    case pat of
      WildcardPat -> f1 ()
      VariablePat x -> f2 x
      Constructor (_, subp) -> r subp
      TuplePat values -> foldl (\i subp -> (r subp + i) 0 values
      _ -> 0
      
countWildcards = g (\() ->) (\_ -> 0)

countWildAndVariableLengths = g (\() -> 1) (\x -> (length x))

countAVar (str, pat) = g (\() -> 0) (\name -> if str == name then 1 else 0) pat

checkPat pat =
  let
    findVars p =
      case p of
        VariablePat name -> [name]
        ConstructorPat (_, cpat) -> findVars cpat
        TuplePat vals -> foldl (\acc el -> (findVars el) ++ acc) [] vals
        _ -> []
    uniqueList [] = True
    uniqueList (x:xs) = 
      if x `elem` xs
      then False
      else uniqueList xs
  in
    (uniqueList . findVars) pat

match (_, WildcardPat) = Just []
match (val, VariablePat n) = Just [(n, val)]
match (Unit, UnitPat) = Just []
match (Constant x, ConstantPat y) = if x == y then Just [] else Nothing
match (Constructor (name, val), ConstructorPat (name', pat)) = 
  if name == name'
  then match (val, pat)
  else Nothing
match (Tuple vals, TuplePat pats) = 
  if (length vals) == (length pats)
  then allAnswers match (zip vals pats)
  else Nothing
match _ = Nothing

firstMatch vals pats = firstAnswer (\el -> match (val, el)) pats

data Type = 
  AnythingType
  | UnitType
  | IntType
  | TupleType ([Type])
  | DataType (String) deriving (Eq, Show)

unifyType t1 AnythingType = Just t1
unifyType AnythingType t2 = Just t2
unifyType IntType IntType = Just IntType
unifyType UnitType UnitType = Just UnitType
unifyType d1@(DataType name1) (DataType name2) = if name1 == name2 then Just d1 else Nothing
unifyType (TupleType types1) (TupleType types2) =
  if (length types1) == (length types2)
  then
    case (allAnswers (\(x,y) ->
      case unifyType x y of
        Just t -> Just [t]
        _ -> Nothing) (zip types1 types2)) of
    of
      Just tys -> Just (TupleType tys)
      Nothing -> Nothing
  else Nothing
unifyType _ _ = Nothing

   










