-module(exceptions).
-compile(export_all).

%% assumes only throws
%% if erlang:exit or erlang:error will be raised inside F
%% the control flow will go to the caller scope and rethrowed here 
%% to the system output
throws(F) ->
  try F() of
      _ -> ok
  catch
      Throw -> {throw, caught, Throw}
  end.

errors(F) ->
  
  try F() of
      _ -> ok
  catch
      error:Error -> {error, caught, Error}
  end.


exits(F) ->
  
  try F() of
      _ -> ok
  catch
      exit:Exit -> {exit, caught, Exit}
  end.

%% how to handle exceptions in two functions
talk() -> "Hi there"

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    end.

whoa() ->
    try
        talk(),
        _Knight = "None shall pass!",
        _Doubles = [N*2 || N <- lists:seq(1, 100)],
        throw(up),
        _WillReturnThis = tequila
    of
        tequila -> "Hey, this worked!"
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

arith_catcher(X, Y) -> 
    case (catch X/Y) of
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.

