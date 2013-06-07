-module(if_stmt).
-compile(export_all).
-author("Rustem [@anguis] Kamun").

oh_god(N) ->
    if N =:= 2 -> might_succeed;
    true -> always_does end.


help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == beef -> "moo";
              Animal == dog -> "bark";
              Animal == tree -> "bark";
              true -> "gjgfggfdg" end,
    {Animal, "says " ++ Talk ++ "!"}.

%% case stmts
insert(X, []) ->
    [X];

insert(X, Set) ->
    case lists:member(X, Set) of
        true -> Set;
        false -> [X | Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celcius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->  'avoid beach. Event the forecast is undetermined.'
    end.