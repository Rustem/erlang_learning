-module(runner).
-export([client_test/1]).
-include("../gen/JobService.hrl").

launch_orber(Name) ->
		mnesia:start(),
		corba:orb_init([{domain, Name},
									  {orber_debug_level, 10},
									  {iiop_port, 0}]),
		orber:install([node()],
				[{ifr_storage_type, ram_copies},
				{nameservice_storage_type, ram_copies}]),
		orber:start().

readIOR(FileName) ->
		{ok, Binary} = file:read_file(FileName),
		erlang:binary_to_list(Binary).


client_test(["ior", IORFile, "handle_job", Job=#'JobService_job'{}]) ->
		launch_orber("SimpleClient"),
		oe_JobService:oe_register(),
		Obj = corba:string_to_object(readIOR(IORFile)),
		Res = 'JobService_JobHandler':handle_job(Job),
		io:format("JOB HANDLED: ~p~n", [Res]).