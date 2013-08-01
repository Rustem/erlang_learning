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

get_empty_component(CosName) ->
		NS = corba:resolve_initial_references("NameService"),
		NC = lname_component:set_id(lname_component:create(), CosName),
		N = lname:insert_component(lname:create(), 1, NC),
		{NS, N}.

system_log(ServiceName, Msg, Args) ->
		io:format("~p SERVICE: " ++ Msg ++ ".~n", [ServiceName | Args]).