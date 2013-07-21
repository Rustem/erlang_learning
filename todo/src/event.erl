-module(event).
-compile(export_all).
-record(state, {server,
								name="",
								to_go=0}).

start(EventName, DateTime) ->
		spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
		spawn_link(?MODULE, init, [self(), EventName, DateTime]).

%%% event's innards
init(Server, EventName, DateTime) ->
		loop(#state{server=Server,
								name=EventName,
								to_go=time_to_go(DateTime)}).


loop(S = #state{server=Server, to_go=[T|Next]}) ->
		%% Loop uses a list for times in order to go around the ~49 days limit
		%% on timeouts.
		receive
				{Server, Ref, cancel} ->
						Server ! {Ref, ok}
		after timer:seconds(T) ->
				if Next =:= [] ->
						Server ! {done, S#state.name};
					 Next =/= [] ->
					 	loop(S#state{to_go=Next})
				end
		end.

normalize(N) ->
		Limit = 49 * 24 * 60 * 60,
		[N rem Limit | lists:duplicate(N div Limit, Limit)].


time_to_go(Timeout={{_, _, _}, {_, _, _}}) ->
		Now = calendar:local_time(),
		ToGo = calendar:datetime_to_gregorian_seconds(Timeout) - 
				calendar:datetime_to_gregorian_seconds(Now),
		Secs = if ToGo > 0 -> ToGo;
							ToGo =< 0 -> 0
					 end,
		normalize(Secs).

cancel(Pid) ->
		%% Monitor in case the process is already dead.
		Ref = erlang:monitor(process, Pid),
		Pid ! {self(), Ref, cancel},
		receive
				{Ref, ok} ->
						erlang:demonitor(Ref, [flush]),
						ok;
				{'DOWN', Ref, process, Pid, _Reason} ->
						ok
		end.
