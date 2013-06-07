%% higher order functions

-module(hhfuns).
-compile(export_all).
-author("Rustem [@anguis] Kamun").

%% map function

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.


% filter
filter(_, []) -> [];
filter(Pred, [X|T]) ->
    case Pred(X) of
        true -> [X | filter(Pred, T)];
        false -> filter(Pred, T)
    end.


% tail filter


tail_filter(Pred, L) -> lists:reverse(tail_filter(Pred, L, [])).

tail_filter(_, [], Acc) -> Acc;
tail_filter(Pred, [X|T], Acc) ->
    case Pred(X) of
        true -> tail_filter(Pred, T, [X|Acc]);
        false -> tail_filter(Pred, T, Acc)
    end.

fold(_, Acc, []) -> Acc;
fold(F, Acc, [X|T]) -> fold(F, F(X, Acc), T).


%% we are given with any X from T, then
%% Pred(X) -> True 

all(Pred, T) -> all(Pred, T, true).
all(_, [], State) -> State;
all(Pred, [X|T], State) ->
    all(Pred, T, Pred(X) and State).