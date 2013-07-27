-module(runner).
-compile(export_all).
-include("../gen/JobService.hrl").

% NOTE. Launch servers (Job Handlers)
% NOTE. Implement handlers
% NOTE. Support pipelining {submit: ObjRef1, validate: ObjRef2} in Env
% NOTE. Supervise and restart in case of failure
% NOTE. Support pubsub


launch_app(DomainName) ->
		launch_orber(DomainName),
		oe_JobService:oe_register().

launch_orber(Name) ->
		mnesia:start(),
		try 
				corba:orb_init([
						{domain, Name},
					  {orber_debug_level, 10},
					  {iiop_port, 0}])
		catch
				exit:_ ->
						io:format("Orber already configured.")
		end,
		reinstall_orber().
		

reinstall_orber() ->
		ok = orber:uninstall(),
		orber:stop(),
		orber:install([node()],
				[{ifr_storage_type, ram_copies},
				{nameservice_storage_type, ram_copies}]),
		orber:start().


client_test() ->
		NS = corba:resolve_initial_references("NameService"),
		NC = lname_component:set_id(lname_component:create(), "JobService"),
		N = lname:insert_component(lname:create(), 1, NC),
		case catch 'CosNaming_NamingContext':resolve(NS, N) of
				{'EXCEPTION', E} ->
						io:format("The job server is not registered.~nDetails: ~p", [E]);
				JobObj ->
						FakeJob = #'JobService_job'{'Title'="Do prototype"},
						Res = 'JobService_JobHandler':handle_job(JobObj, FakeJob),
						io:format("Job submitted with result: ~p ~n", [Res])
		end.
		% oe_JobService:oe_register(),
		% Obj = corba:string_to_object(readIOR(IORFile)),
		% Job=#'JobService_job'{'Title'="Do job"},
		% Res = 'JobService_JobHandler':handle_job(Obj, Job),
		% io:format("JOB HANDLED: ~p~n", [Res]).

server_test() ->
		JobObj = 'JobService_JobHandler':oe_create(),
		NS = corba:resolve_initial_references("NameService"),
		NC = lname_component:set_id(lname_component:create(), "JobService"),
		N = lname:insert_component(lname:create(), 1, NC),
		'CosNaming_NamingContext':bind(NS, N, JobObj).
		% writeIOR(IORFile, corba:object_to_string(Obj)).

% writeIOR(FileName, IOR) ->
% 		{ok, FileDesc} = file:open(FileName, [write]),
% 		file:write(FileDesc, IOR),
% 		file:close(FileDesc).

% readIOR(FileName) ->
% 		{ok, Binary} = file:read_file(FileName),
% 		erlang:binary_to_list(Binary).