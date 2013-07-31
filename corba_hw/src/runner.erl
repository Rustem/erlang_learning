-module(runner).
-compile(export_all).
-include("../gen/JobService.hrl").
-include("records.hrl").
-define(SUBMIT_SRV, 'JobService_Submitable').
-define(VALIDATE_SRV, 'JobService_Validateable').
-define(PROCESS_SRV, 'JobService_Processable').
-define(STORE_SRV, 'JobService_Storeable').


-define(DEFAULT_REGNAME, "JobHandler").

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


client_test(Env) ->
		{SrvInfo, SubmitSrvObj} = case commons:get_srv_obj(Env, submit_srv) of
				{bad, Reason} -> throw(Reason);
				{ok, Result} -> Result
		end,

		#service{cosname=CN, stub_module=SM} = SrvInfo,

		FakeJob = #'JobService_job'{
				title="Do prototype",
				salary=450000,
				currency=kzt,
				country=kazakhstan,
				reqments=["one", "two", "three", "four"],
				job_details=["1", "2", "3", "4"]},

		Res = SM:submit_job(
				SubmitSrvObj, FakeJob),
		io:format("Job submitted with result: ~p ~n", [Res]).

		% oe_JobService:oe_register(),
		% Obj = corba:string_to_object(readIOR(IORFile)),
		% Job=#'JobService_job'{'Title'="Do job"},
		% Res = 'JobService_JobHandler':handle_job(Obj, Job),
		% io:format("JOB HANDLED: ~p~n", [Res]).

server_test() ->
		{ok, Env} = build_env(),
		io:format("Service initialized with env: ~p", [Env]),
		Env.

build_env() ->
		SubmitServiceInfo = #service{
				cosname=atom_to_list(?SUBMIT_SRV),
				stub_module=?SUBMIT_SRV,
				regname=?SUBMIT_SRV},
		ValidServiceInfo = #service{
				cosname=atom_to_list(?VALIDATE_SRV),
				stub_module=?VALIDATE_SRV,
				regname=?VALIDATE_SRV},
		ProcessServiceInfo = #service{
				cosname=atom_to_list(?PROCESS_SRV),
				stub_module=?PROCESS_SRV,
				regname=?PROCESS_SRV},
		StoreServiceInfo = #service{
				cosname=atom_to_list(?STORE_SRV),
				stub_module=?STORE_SRV,
				regname=?STORE_SRV},
		
		Env = orddict:from_list([
				{submit_srv, SubmitServiceInfo},
				{valid_srv, ValidServiceInfo},
				{process_srv, ProcessServiceInfo},
				{store_srv, StoreServiceInfo}
		]),
		
		init_service(SubmitServiceInfo, Env),
		init_service(ValidServiceInfo, Env),
		init_service(ProcessServiceInfo, Env),
		init_service(StoreServiceInfo, Env),
		{ok, Env}.

init_service(#service{cosname=CosName, stub_module=StubModule, regname=RegName}, Env) ->
		SrvObjectKey = StubModule:oe_create_link(Env, [{regname, {local, RegName}}]),
		NS = corba:resolve_initial_references("NameService"),
		NC = lname_component:set_id(lname_component:create(), CosName),
		N = lname:insert_component(lname:create(), 1, NC),
		'CosNaming_NamingContext':bind(NS, N, SrvObjectKey),
		io:format("Service ~p launched and bind to ~p.~n", [RegName, CosName]).

% writeIOR(FileName, IOR) ->
% 		{ok, FileDesc} = file:open(FileName, [write]),
% 		file:write(FileDesc, IOR),
% 		file:close(FileDesc).

% readIOR(FileName) ->
% 		{ok, Binary} = file:read_file(FileName),
% 		erlang:binary_to_list(Binary).