-module(recursion_gs).
-compile(export_all).

%% factorial
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N * fac(N - 1).


%% length of list

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

%% tail recursion

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).


%% tail recursion with len
tail_len(T) -> tail_len(T, 0).

tail_len([], Acc) -> Acc;
tail_len([X | T], Acc) -> tail_len(T, 1 + Acc).

%% reverse

reverse([]) -> [];
reverse([X | T]) -> reverse(T) ++ [X].

tail_reverse(T) -> tail_reverse(T, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([X | T], Acc) -> tail_reverse(T, [X|Acc]).

%% sublist example
%% given T, and N => return [1...N] of T

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([X|T], N) when N > 0 -> [X|sublist(T, N-1)].

tail_sublist(T, N) -> tail_reverse(tail_sublist(T, N, [])).

tail_sublist([], _, Acc) -> Acc;
tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([X|T], N, Acc) when N > 0 -> tail_sublist(T, N - 1, [X | Acc]).


%% zip function, We are given with T1, T2
%% return [{A11, A21}, {B12, B22}, ...]
zip([], []) -> [];
zip([X|T1], [Y|T2]) -> [{X, Y} | zip(T1, T2)].

tail_zip(T1, T2) -> reverse(tail_zip(T1, T2, [])).
tail_zip([], [], Acc) -> Acc;
tail_zip([X|T1], [Y|T2], Acc) -> tail_zip(T1, T2, [{X, Y} | Acc]).


%% zipN
zipN([]) ->
    [];

zipN(ListOfLists) ->
    zipN([], ListOfLists, [], []).


zipN(Ts, [], E, Acc) ->
    zipN([], lists:reverse(Ts), [], [list_to_tuple(lists:reverse(E)) | Acc]);

zipN(Ts, [[Head | Tail] | ListOfLists], E, Acc) ->
    zipN([Tail | Ts], ListOfLists, [Head | E], Acc);

zipN([], _, [], Acc) ->
    lists:reverse(Acc).

%% quicksort
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [X|Rest], Smaller, Larger) ->
    if X =< Pivot -> partition(Pivot, Rest, [X|Smaller], Larger);
       X > Pivot -> partition(Pivot, Rest, Smaller, [X|Larger])
    end.


%% another quicksort
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <-Rest, Smaller =< Pivot])
    ++ [Pivot] ++
    lc_quicksort([Greater || Greater <-Rest, Greater >= Pivot]).



% since base case is:
% S0 = (Fib0, Fib1) = (0, 1)
% for n >= 1 succ(Fibn-1, Fibn) = (Fibn, Fibn+1) = (Fibn, Fibn-1 + Fibn)


fibo_tail(0, Result, _Next) -> Result;
fibo_tail(N, Result, Next) when N > 0 -> fibo_tail(N-1, Next, Result + Next).
fibo(N) -> fibo_tail(N, 0, 1).

