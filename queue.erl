-module(queue).
-export([new/0, add/2, fetch/1, len/1]).
-author("Rustem [@anguis] Kamun").

% initiates top and tail
new() ->
  {[], []}.

% add item to top
add(Item, {X, Y}) ->
  {[Item|X], Y}.

fetch({X, [H|T]}) -> 
  {ok, H, {X,T}};

fetch({[], []}) -> 
  empty;

fetch({X, []}) -> 
  % Perform this heavy computation only sometimes.
  fetch({[],lists:reverse(X)}).

len({X, Y}) ->
  length(X) + length(Y).