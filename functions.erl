-module(functions).
-compile(export_all).

head([X|_]) -> X.

second([_, X | _]) -> X.


%% for L = [3, 4, 4, 5, 6] returns 4

same_as_prev([X | [] ]) ->
   {bad, "No such couple."};

same_as_prev([X, X | _]) ->
   X;

same_as_prev([_, Y | L]) ->
   same_as_prev([Y | L]).


%% more advanced ex with guards
valid_time(Date = {Y, M, D}, Time = {H, Min, S}) ->
    io:format(
        "The date tuple (~p) says today is: ~p/~p/~p, ~n", 
        [Date, Y, M, D]
    ),
    io:format(
        "The time tuple (~p) indicates: ~p:~p:~p.~n",
        [Time, H, Min, S]
    );

valid_time(_, _) ->
    io:format("Stop feeding me wrong data!~n").

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >= 16, X =< 104 ->
    true;
right_age(_) ->
    false.

wrong_age(X) when X < 16; X > 104 ->
    true;
wrong_age(_) ->
    false.
