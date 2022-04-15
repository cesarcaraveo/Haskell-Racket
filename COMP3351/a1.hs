
-- Cesar Caraveo
-- COMP 3351: Programming Languages
-- 28 March 2018

module A1b where

-- sDotProduct :: Num
-- takes single number and 2 pairs
-- multiply 1st coordinates together and add them two product of 2nd coordinates and multiply result by number
-- ex. 5 (3,4) (4,5) = ((3*4) + (4*5)) * 5 = 160

sDotProduct num (f1, f2) (s1, s2) = ((f1 * s1) + (f2 * s2)) * num

-- distance :: Floating
-- takes 2 pairs and returns cartesian distance between them
-- d = sqrt{(x2 - x1)^2 + (y2 - y1)^2}

distance (x1, y1) (x2, y2) = sqrt (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)))

-- triple distance :: Floating
-- takes 2, 3 tuples (x, y, z) and returns distance between them
-- d = sqrt{(x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2}

tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)) + ((z2 - z1) * (z2 - z1)))

-- findMax :: Num
-- takes list and returns pair where first element is min and second is max

findMax list = if null list
  then 0
  else findGreatest (head list) list

-- findGreatest :: Num
-- helper function for findMax 
-- takes in a list and a max value and returns the maximum value from the list.
-- compares max input to head of list

findGreatest max list = if null list
  then max
  else findGreatest (whichIsGreater max (head list)) (tail list)

-- whichIsGreater :: Num
-- helper function for findGreatest
-- compares two values and returns the greater one

whichIsGreater a b = if a < b
  then b
  else a

-- tupleDotProduct :: Num
-- takes 2 lists of numbers and returns dot product between them (both are same size)
-- dot(q, p) = (q_1 * p_1) + (q_2 * p_2) + ... + (q_n * p_n)

tupleDotProduct list1 list2 = if null list1 || null list2
  then 0
  else ((head list1) * (head list2)) + tupleDotProduct (tail list1) (tail list2) 

-- revZip2Lists :: [(a, b)]
-- takes 2 lists and returns list with pair from each list but in reverse
-- ex. [1,2,3] [a, b, c] = [(c, 3), (b, 2), (a, 1)]

revZip2Lists list1 list2 = if null list1 || null list2
  then []
  else revZip2Lists (tail list1)(tail list2)++[(head list2, head list1)]

-- everyThird :: [a]
-- takes list and returns new list only consisting of every third element
-- ex. [1, 2, 3, 4, 5, 6] = [3, 6]

everyThird list = if length list < 3
  then []
  else [list !! 2] ++ everyThird (drop 3 list)

-- everyK :: [a]
-- takes list and number k and returns every kth element of the list

everyK k list = if length list < k
  then []
  else [list !! (k-1)] ++ everyK k (drop k list)

