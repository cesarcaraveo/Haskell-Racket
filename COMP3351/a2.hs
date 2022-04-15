module A2b where
-- removeAllExcept
-- returns list that removes everything that is not equal to the given element
-- ex. removeAllExcept 'a' ['b', 'a', 'c', 'a'] returns ['a', 'a']

removeAllExcept element list = 
  case list of
    [] -> []
    (first:rest) -> if (element == first) then first : removeAllExcept element rest else removeAllExcept element rest

-- removeAll
-- returns list that removes all occurrences of the given element
-- ex. removeAll 'a' ['a', 'b', 'a', 'c'] returns ['b', 'c']

removeAll element list = 
  case list of
    [] -> []
    (first:rest) -> if (element == first) then removeAll element rest else first : removeAll element rest
    
-- substitute
-- replace all occurrences of first arg with second arg in list and return new list
-- ex. substitute 3 4 [1,2,3,4] returns [1,2,4,4]

substitute f1 f2 list = 
  case list of
    [] -> []
    (first:rest) -> if (first == f1) then f2 : substitute f1 f2 rest else first : substitute f1 f2 rest

-- mergeSorted3 
-- takes 3 lists in sorted order and merges them to return final list which is sorted in increasing order
-- ex. [2,3,5] [1,8] [-1, 0, 4, 10] returns [-1, 0, 1, 2, 3, 4, 5, 8, 10]

mergeSorted3 x y z = 
  let
     merge [] list = list
     merge list [] = list
     merge (x:xs) (y:ys) = 
       if (x <= y)
       then x : merge (xs) (y:ys)
       else y : merge (x:xs) (ys)
  in
    merge (x) (merge y z)

-- Trees 

-- TriTree is either an empty node or a tri node with 3 TriTrees
data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a) deriving (Show)

-- added from assignment
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False
  -- ______________________
  
-- nodeValue
-- takes trinary tree and returns value of the given node or error if an emptyNode

nodeValue tree = 
  case tree of
    EmptyNode -> error "empty node"
    TriNode a l m r -> a

--leftChild
-- takes trinary tree and returns left child or error if an emptyNode

leftChild tree = 
  case tree of
    EmptyNode -> error "empty node"
    TriNode a l m r -> l

--middleChild
-- takes trinary tree and returns middle child or error if an emptyNode

middleChild tree = 
  case tree of
    EmptyNode -> error "empty node"
    TriNode a l m r -> m

-- rightChild
-- takes trinary tree and returns right child or error if an empty node

rightChild tree = 
  case tree of
    EmptyNode -> error "empty node"
    TriNode a l m r -> r

-- inTree
-- returns True or False if given element is in Trinary Tree
-- ex. inTree 5 t returns true if 5 is in it or false otherwsie

--inTree element EmptyNode = False
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

-- leafList
-- takes trinary tree and returns list with all values in the leaves of the tree
-- a leaf is a node with three empty branches

leafList tree = 
  case tree of
    EmptyNode -> []
    TriNode a (EmptyNode) (EmptyNode) (EmptyNode) -> [a]
    TriNode a l m r -> leafList l ++ leafList m ++ leafList r  

-- inOrderMap
-- acts like map on a list
-- performs in-order traversal of tree
-- takes a function that takes type a and returns type b
-- pass in a trinarytree with type a and returns a trinarytree of type b
-- trees match in structure but each value will be transformed

inOrderMap function tree = 
  case tree of 
    EmptyNode -> EmptyNode
    TriNode a (EmptyNode) (EmptyNode) (EmptyNode) -> TriNode (function a) (EmptyNode) (EmptyNode) (EmptyNode)
    TriNode a l (EmptyNode) (EmptyNode) -> TriNode (function a) (inOrderMap function l) EmptyNode EmptyNode
    TriNode a EmptyNode m EmptyNode -> TriNode (function a) EmptyNode (inOrderMap function m) EmptyNode
    TriNode a EmptyNode EmptyNode r -> TriNode (function a) EmptyNode EmptyNode (inOrderMap function r)
    TriNode a l m EmptyNode -> TriNode (function a) (inOrderMap function l) (inOrderMap function m) EmptyNode
    TriNode a l EmptyNode r -> TriNode (function a) (inOrderMap function l) EmptyNode (inOrderMap function r)
    TriNode a EmptyNode m r -> TriNode (function a) EmptyNode (inOrderMap function m) (inOrderMap function r)
    TriNode a l m r -> TriNode (function a) (inOrderMap function l) (inOrderMap function m) (inOrderMap function r)

-- preOrderFold
-- similar to fold
-- takes a function as first argument (accumulator value) and a TriTree
-- performs pre-order walk of the tree, applying function to each value
-- uses result of that function in next call of folding in the tree
-- ex. a tree with (1 2 4 3) where 2,4,3 are children, then preOrderFold (+) 0 (1 2 4 3) results in 10

preOrderFold function acc tree = 
  case tree of
    EmptyNode -> acc
    TriNode a l m r ->  let
                                    newAcc = (function acc a)
                                  in
                                    let
                                      leftBranch = preOrderFold function newAcc l
                                    in
                                      let
                                        middleBranch = preOrderFold function leftBranch m
                                      in
                                        preOrderFold function middleBranch r

-- achievements

-- TreePath
-- can either be a path left, path middle, or pathright
data TreePath = PathLeft |  PathMiddle | PathRight

path p = 
findPath element = 

-- findAndPrune
-- takes a value and a tree and retuns a tree with that branch and subtree removed

findAndPrune value tree =
  let
    treeUpToPoint = tree
  in
    if (inTree value treeUpToPoint)
    then case tree of
            EmptyNode -> EmptyNode
            TriNode a l m r -> if (inTree value l)
                                         then TriNode a EmptyNode m r
                                         else 
                                           if (inTree value m)
                                           then TriNode a l EmptyNode r
                                           else TriNode a l m EmptyNode
    else error "value not in tree"
  
-- pruneWithPath [] tree = tree
-- pruneWithPath (x:[]) tree = 
--   case x of
--     PathLeft -> 
-- pruneWithPath (x:xs) tree = 
--   case tree of
--     EmptyNode -> EmptyNode
--     TriNode a l m r -> case x of
--                                    PathLeft -> pruneWithPath xs l
--                                    PathRight -> pruneWithPath xs r
--                                    PathMiddle -> pruneWithPath xs m

