-module(hotload).
-export([server/1]).

server(State) ->
		receive
				update ->
						NewState = ?MODULE:upgrade(State),
						?MODULE:server(NewState);  %% Loop in the new version of the module
				SomeMessage ->
						server(State)
		end.