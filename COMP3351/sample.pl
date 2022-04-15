shirt(cupids).
shirt(happy_faces).
shirt(leprechauns).
shirt(reindeer).

/* we don't actually use this information, it's just there to show the relationship */
colleague(crow).
colleague(evans).
colleague(hurley).
colleague(speigler).

/* the kinds of relatives we have in the puzzle */
relative(daughter).
relative(father_in_law).
relative(sister).
relative(uncle).

solve :-
    shirt(CrowShirt), shirt(EvansShirt), shirt(HurleyShirt), shirt(SpeiglerShirt),
    all_different([CrowShirt, EvansShirt, HurleyShirt, SpeiglerShirt]),
    
    relative(CrowRelative), relative(EvansRelative),
    relative(HurleyRelative), relative(SpeiglerRelative),
    all_different([CrowRelative, EvansRelative, HurleyRelative, SpeiglerRelative]),

    Triples = [ [crow, CrowShirt, CrowRelative],
                [evans, EvansShirt, EvansRelative],
                [hurley, HurleyShirt, HurleyRelative],
                [speigler, SpeiglerShirt, SpeiglerRelative] ],
   
    % 1. The shirt with the grinning leprechauns wasn't a present from a daughter.
    \+ member([_, leprechauns, daughter], Triples),
    
    % 2. Colleague Crow's shirt features neither the dancing reindeer nor the yellow happy faces.
    \+ member([crow, reindeer, _], Triples),
    \+ member([crow, happy_faces, _], Triples),
    
    % 3. Colleague Speigler's shirt wasn't a present from his uncle.
    \+ member([speigler, _, uncle], Triples),
    
    % 4. The shirt with the yellow happy faces wasn't a gift from a sister.
    \+ member([_, happy_faces, sister], Triples),
    
    % 5. Colleague Evans and Colleague. Speigler own the shirt with the grinning leprechauns
    %    and the shirt that was a present from a father-in-law, in some order.
    ( (member([evans, leprechauns, _], Triples),
       member([speigler, _, father_in_law], Triples)) ;
       
      (member([speigler, leprechauns, _], Triples),
       member([evans, _, father_in_law], Triples)) ),
    
    % 6. Colleague. Hurley received his flamboyant shirt from his sister.
    member([hurley, _, sister], Triples),
    
    % print out messages about who got what from whom
    tell(crow, CrowShirt, CrowRelative),
    tell(evans, EvansShirt, EvansRelative),
    tell(hurley, HurleyShirt, HurleyRelative),
    tell(speigler, SpeiglerShirt, SpeiglerRelative).

% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(X, Y, Z) :-
    write('Colleague '), write(X), write(' got the '), write(Y),
    write(' shirt from a '), write(Z), write('.'), nl.