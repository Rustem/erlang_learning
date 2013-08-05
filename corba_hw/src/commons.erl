-module(commons).
-compile(export_all).

-include("../gen/JobService.hrl").
-include("records.hrl").

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : get_srv_obj/2
%% Arguments  : Services - list of #service{} record
%%              Key = 'submit_srv'|'valid_srv'|'process_srv'|'store_srv'
%% Returns    : ReturnValue = {bad, Reason} | {ok, #service{}, CorbaObj}
%%              OE_Reply = Status
%%              Status = 'success' | 'fail' 
%% Raises     : JobService_HandlerNotRegistered
%% Description: Returns Service Handler Object by Key.


get_srv_obj(Services, Key) ->
		case orddict:find(Key, Services) of
				error ->
						{bad, "Meta of Service " ++ atom_to_list(Key) ++ " is not included into record #service{}."};
				{ok, SrvInfo} ->
						{NS, N} = get_empty_component(SrvInfo#service.cosname),
						case catch 'CosNaming_NamingContext':resolve(NS, N) of
								{'EXCEPTION', E} ->
										{bad, "Service[" ++ SrvInfo#service.cosname ++ " is not registered."};
								Obj -> {ok, {SrvInfo, Obj}}
						end
		end.

%%----------------------------------------------------------------------
%% Function   : get_srv_obj/1
%% Arguments  : CosName - binary, name of naming context
%% Returns    : ReturnValue = {NS, N}
%%              NS = initial name service
%%              N = naming context with CosName
%% Description: Returns new naming context to bind object to
get_empty_component(CosName) ->
		NS = corba:resolve_initial_references("NameService"),
		NC = lname_component:set_id(lname_component:create(), CosName),
		N = lname:insert_component(lname:create(), 1, NC),
		{NS, N}.


%%----------------------------------------------------------------------
%% Function   : system_log/3
%% Arguments  : ServiceName = binary, name of service
%%              Msg = binary, message to log
%%							Args = list, additional arguments to message
%% Returns    : ReturnValue = nothing
%% Description: Logs a system message to console
system_log(ServiceName, Msg, Args) ->
		io:format("~p SERVICE: " ++ Msg ++ ".~n", [ServiceName | Args]).