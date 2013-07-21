-module(naming_processes).
-compile(export_all).

start_critic() ->
		spawn(?MODULE, critic, []).


start_critic2() ->
		spawn(?MODULE, restarter, []).

restarter() ->
		process_flag(trap_exit, true),
		Pid = spawn_link(?MODULE, critic2, []),
		register(critic, Pid),
		receive
				{'EXIT', Pid, normal} ->
						ok;
				{'EXIT', Pid, shutdown} ->
						ok;
				{'EXIT', Pid, _} ->
						restarter()
			end.

judge(Pid, Band, Album) ->
		critic	! {self(), {Band, Album}},
		Pid = whereis(critic),
		receive
				{Pid, Criticism} -> Criticism
		after 2000 ->
				timeout
		end.

critic() ->
		receive
				{From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
						From ! {self(), "They are great!"};
				{From, {"System of a Downtime", "Memoize"}} ->
						From ! {self(), "They are not Johny Crash but they're good."};
				{From, {"Johnny Crash", "The token ring of fire."}} ->
						From ! {self(), "Simply incredible."};
				{From, {_Band, _Album}} ->
						From ! {self(), "They are incredible!"}
		end,
		critic().

judge2(Band, Album) ->
		r"""Using Refs helps to make code safe from race conditions.
		Especially when message tagging depends on Pid of sending proccess."""
		Ref = make_ref(),
		critic ! {self(), Ref, {Band, Album}},
		receive
				{Ref, Criticism} -> Criticism
		after 2000 ->
				timeout
		end.

critic2() ->
		r"""Responds with critic about message.
		Message tagged with Refs."""
		receive
				{From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
						From ! {Ref, "They are great!"};
				{From, Ref, {"System of a Downtime", "Memoize"}} ->
						From ! {Ref, "They are not Johny Crash but they're good."};
				{From, Ref, {"Johnny Crash", "The token ring of fire."}} ->
						From ! {Ref, "Simply incredible."};
				{From, Ref, {_Band, _Album}} ->
						From ! {Ref, "They are terrible!"}
		end,
		critic().