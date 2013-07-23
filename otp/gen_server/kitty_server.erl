-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

start_link() -> spawn_link(fun init/0).

order_cat(Pid, Name, Color, Description) ->
		my_server:call(Pid, {order, Name, Color, Description}).


return_cat(Pid, Cat = #cat{}) ->
		Pid ! {return, Cat},
		ok.

close_shop(Pid) ->
		my_server:call(Pid, terminate).

init() -> loop([]).

loop(Module, Cats) ->
		receive
				{async, Msg} ->
						loop(Module, Module:handle_cast(Msg, State));
				{sync, Pid, Ref, Msg} ->
						loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
		end.

make_cat(Name, Col, Desc) ->
		#cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
		[io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
		ok.
