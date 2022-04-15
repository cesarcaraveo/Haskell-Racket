% this is a comment
/* so is this */

% 3 facts in the world
likes(wallace, cheese). % must end with a period, all lowercase
likes(grommit, cheese).
likes(wendolene, sheep).

% capitals mean its a variable
% \+ is (not), if X likes Z and Y likes Z, then X and Y are friends
friend(X, Y) :- \+(X = Y), likes(X, Z), likes(Y, Z). % comma is like logical and

food_type(velveeta, cheese).
food_type(ritz, crackers).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

% rule
food_flavor(X, Y) :- food_type(X, Z), flavor(Y, Z).

% more rules
different(red, green).
different(green, red).

different(red, blue).
different(blue, red).

different(green, blue).
different(blue, green).

% :- means function body follows
coloring(Alabama, Mississippi, Georgia, Tennessee, Florida) :- 
different(Mississippi, Tennessee),
different(MIssissippi, Alabama),
different(Alabama, Tennessee),
different(Alabama, Georgia),
different(Alabama, Florida),
different(Georgia, Florida),
different(Georgia, Tennessee).

cat(lion).
cat(tiger).
cat(bobcat).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.
twin_cats(X, Y) :- cat(X), cat(Y).

father(aegon, aerys).
father(aerys, daenerys).

% recursion (first line is base case)
ancestor(X, Y) :- father(X, Y).
ancestor(X, Y) :- father(X, Z), ancestor(Z, Y).

% count of the empty list is 0
count(0, []).
% go through list and count 
count(Count, [First|Rest]) :- count(TailCount, Rest), Count is TailCount + 1.

% sums elements in list
sum(0, []).
sum(Sum, [Head|Tail]) :- sum(TailSum, Tail), Sum is TailSum + Head.

% average of list
average(Average, List) :- sum(Sum, List), count(Count, List), Average is Sum/Count.

% first and second parameters are lists, third is answer from concatenating
concat([], List, List). 
% first of first list will be in final list
concat([First|Rest], List, [First|Rest2]) :- concat(Rest, List, Rest2).

factorial(0, 1). % factorial of 0 is one
factorial(N, F) :-
  N > 0 % n must be greater than 0
  N1 is N - 1, % subtract one from N and store it in N1
  factorial(N1, F1), % call factorial on N1 and store result in F1
  F is N * F1. % multiply N * F1
  
% puzzle and solution should unify (thats what equals means)
sudoku(Puzzle, Solution) :- 
  Solution = Puzzle,
  Puzzle = [S11, S12, S13, S14,
                  S21, S22, S23, S24,
                  S31, S32, S33, S34,
                  S41, S42, S43, S44],
                  
  Row1 = [S11, S12, S13, S14],
  Row2 = [S21, S22, S23, S24],
  Row3 = [S31, S32, S33, S34],
  Row4 = [S41, S42, S43, S44],
  
  Col1 = [S11, S21, S31, S41],
  Col2 = [S12, S22, S32, S42],
  Col3 = [S13, S23, S33, S43],
  Col4 = [S14, S24, S34, S44],
  
  Sqr1 = [S11, S12, S21, S22],
  Sqr2 = [S13, S14, S23, S24],
  Sqr3 = [S31, S32, S41, S42],
  Sqr4 = [S33, S34, S43, S44],
  
  valid([Row1, Row2, Row3, Row4,
            Col1, Col2, Col3, Col4,
            Sqr1, Sqr2, Sqr3, Sqr4]),
  
  fd_domain(Puzzle, 1, 4). % domain of puzzle is 1 to 4

valid([]).
valid([Head|Tail]) :- 
  fd_all_different(Head), % makes sure everything is different
  valid(Tail).
  
double(X, Y) :- Y is X * 2.

% finding max in list
accMax([H|T], A, Max) :-
  H > A,
  accMax(T, H, Max).

accMax([H|T], A, Max) :-
  H =< A,
  accMax(T, A, Max).

acc([], A, A).

vertical(line(point(X, Y), point(X, Z))).
horizontal(line(point(X,Y), point(Z, Y))).

f(a).
f(b).

g(a).
g(b).

h(b).

k(X) :- f(X), g(X), h(X). % x must be b
  




