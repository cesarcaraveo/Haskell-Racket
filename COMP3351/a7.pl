% citizens
citizen(kadja).
citizen(caledor).
citizen(crix).
citizen(enrik).

% illnesses
illness(void_fever).
illness(murlok_madness).
illness(demon_spots).
illness(butter_toes).

% recipes
recipe(lichen_kings_tea).
recipe(pixie_blush).
recipe(essence_of_durarara).
recipe(dispel_magic).

% locations
location(greyrock_hold).
location(emerald_sanctuary).
location(steelforge).
location(swamp_of_despair).

% solve puzzle
solve :- 
  % all illnesses are different
  illness(KadjaIllness), illness(CaledorIllness), illness(CrixIllness), illness(EnrikIllness),
  all_different([KadjaIllness, CaledorIllness, CrixIllness, EnrikIllness]),
  
  % all recipes are different
  recipe(KadjaRecipe), recipe(CaledorRecipe), recipe(CrixRecipe), recipe(EnrikRecipe),
  all_different([KadjaRecipe, CaledorRecipe, CrixRecipe, EnrikRecipe]),
  
  % all locations are different
  location(KadjaLocation), location(CaledorLocation), location(CrixLocation), location(EnrikLocation),
  all_different([KadjaLocation, CaledorLocation, CrixLocation, EnrikLocation]),
  
  % creates sets 
  Sets = [[kadja, KadjaIllness, KadjaRecipe, KadjaLocation],
              [caledor, CaledorIllness, CaledorRecipe, CaledorLocation],
              [crix, CrixIllness, CrixRecipe, CrixLocation],
              [enrik, EnrikIllness, EnrikRecipe, EnrikLocation]],
              
  /* The recipe for the potion to treat Kadja's illness (which isn't the Pixie Blush recipe) can only be found in the Emerald Sanctuary. */
  member([kadja, _, _, emerald_sanctuary], Sets),
  \+ member([kadja, _, pixie_blush, _], Sets),
  
  /* The potion used to cure Caledor's malady isn't the one found in Steelforge. Crix's illness was easily treated with the Essence of Durarara potion. */
  \+ member([caledor, _, _, steelforge], Sets),
  member([crix, _, esscence_of_durarara, _], Sets),
  
 /* Enrik was diagnosed with Murlok Madness (which was cured by a simple Dispel Magic potion). The potion used to treat Demon Spots isn't the Pixie Blush recipe. */
  member([enrik, murlok_madness, dispel_magic, _], Sets),
  \+ member([_, demon_spots, pixie_blush, _], Sets),
  
  /* The potion recipe found only in the Swamp of Despair (which isn't the Pixie Blush recipe) is used to cure Butter Toes. */
  member([_, butter_toes, _, swamp_of_despair], Sets),
  \+ member([_, _, pixie_blush, swamp_of_despair], Sets),
  
  % print out messages 
  
  tell(kadja, KadjaIllness, KadjaRecipe, KadjaLocation),
  tell(caledor, CaledorIllness, CaledorRecipe, CaledorLocation),
  tell(crix, CrixIllness, CrixRecipe, CrixLocation),
  tell(enrik, EnrikIllness, EnrikRecipe, EnrikLocation).
  
% Succeeds if all elements of the argument list are bound and different.
% Fails if any elements are unbound or equal to some other element.
all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(W, X, Y, Z) :-
    write('Citizen '), write(W), write(' had '), write(X), write(' illness, but cured it with '), write(Y), write(' found only in '), write(Z), write('.'), nl.
  
  
  
  
  
  
  
  
  
  
  
  
  