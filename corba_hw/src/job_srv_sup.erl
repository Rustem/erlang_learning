-module(job_srv_sup).
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, add_service/3]).

start_link() ->
		supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
		RestartStrategy = {one_for_all, 10, 60},
		{ok, {RestartStrategy, []}}.

add_service(SrvRegName, SrvModName, Env) ->
	 % Listener = {ch1, {ch1, start_link, []},
   %          permanent, 2000, worker, [ch1]},
   	InitSrvOpts = [
   			{regname, {local, SrvRegName}},
   			{sup_child, true}],
   	ServiceSpec = {SrvRegName, {SrvModName, oe_create_link, [Env, InitSrvOpts]},
   							permanent, 2000, worker, [SrvModName]},

   	{ok, Pid, SrvRef} = supervisor:start_child(?MODULE, ServiceSpec),
   	{Pid, SrvRef}.
