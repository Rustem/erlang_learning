%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: JobService_Submitable
%% Source: idl/JobService.idl
%% IC vsn: 4.2.27
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('JobService_Submitable').
-ic_compiled("4_2_27").


%% Interface functions
-export([submit_job/2, submit_job/3]).

%% Exports from "JobService::JobHandler"
-export(['_get_hname'/1, '_get_hname'/2, '_set_hname'/2]).
-export(['_set_hname'/3]).

%% Type identification function
-export([typeID/0]).

%% Used to start server
-export([oe_create/0, oe_create_link/0, oe_create/1]).
-export([oe_create_link/1, oe_create/2, oe_create_link/2]).

%% TypeCode Functions and inheritance
-export([oe_tc/1, oe_is_a/1, oe_get_interface/0]).

%% gen server export stuff
-behaviour(gen_server).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

-include_lib("orber/include/corba.hrl").


%%------------------------------------------------------------
%%
%% Object interface functions.
%%
%%------------------------------------------------------------



%%%% Operation: submit_job
%% 
%%   Returns: RetVal
%%   Raises:  JobService::HandlerNotRegistered
%%
submit_job(OE_THIS, JobCtx) ->
    corba:call(OE_THIS, submit_job, [JobCtx], ?MODULE).

submit_job(OE_THIS, OE_Options, JobCtx) ->
    corba:call(OE_THIS, submit_job, [JobCtx], ?MODULE, OE_Options).

%%%% Operation: '_get_hname'
%% 
%%   Returns: RetVal
%%
'_get_hname'(OE_THIS) ->
    corba:call(OE_THIS, '_get_hname', [], ?MODULE).

'_get_hname'(OE_THIS, OE_Options) ->
    corba:call(OE_THIS, '_get_hname', [], ?MODULE, OE_Options).

%%%% Operation: '_set_hname'
%% 
%%   Returns: RetVal
%%
'_set_hname'(OE_THIS, OE_Value) ->
    corba:call(OE_THIS, '_set_hname', [OE_Value], ?MODULE).

'_set_hname'(OE_THIS, OE_Options, OE_Value) ->
    corba:call(OE_THIS, '_set_hname', [OE_Value], ?MODULE, OE_Options).

%%------------------------------------------------------------
%%
%% Inherited Interfaces
%%
%%------------------------------------------------------------
oe_is_a("IDL:JobService/Submitable:1.0") -> true;
oe_is_a("IDL:JobService/JobHandler:1.0") -> true;
oe_is_a(_) -> false.

%%------------------------------------------------------------
%%
%% Interface TypeCode
%%
%%------------------------------------------------------------
oe_tc(submit_job) -> 
	{{tk_enum,"IDL:JobService/Status:1.0","Status",["success","fail"]},
         [{tk_struct,"IDL:JobService/job:1.0","job",
                     [{"title",{tk_string,0}},
                      {"salary",tk_ulong},
                      {"currency",
                       {tk_enum,"IDL:JobService/JobGeoDetails/CurrencyEnum:1.0",
                                "CurrencyEnum",
                                ["kzt","usd","rub","gbp"]}},
                      {"country",
                       {tk_enum,"IDL:JobService/JobGeoDetails/CountryEnum:1.0",
                                "CountryEnum",
                                ["kazakhstan","russia","great_britain",
                                 "usa"]}},
                      {"reqments",{tk_sequence,{tk_string,146},4}},
                      {"job_details",{tk_sequence,{tk_string,146},4}}]}],
         []};
oe_tc('_get_hname') -> 'JobService_JobHandler':oe_tc('_get_hname');
oe_tc('_set_hname') -> 'JobService_JobHandler':oe_tc('_set_hname');
oe_tc(_) -> undefined.

oe_get_interface() -> 
	[{"_get_hname", 'JobService_JobHandler':oe_tc('_get_hname')},
	{"_set_hname", 'JobService_JobHandler':oe_tc('_set_hname')},
	{"submit_job", oe_tc(submit_job)}].




%%------------------------------------------------------------
%%
%% Object server implementation.
%%
%%------------------------------------------------------------


%%------------------------------------------------------------
%%
%% Function for fetching the interface type ID.
%%
%%------------------------------------------------------------

typeID() ->
    "IDL:JobService/Submitable:1.0".


%%------------------------------------------------------------
%%
%% Object creation functions.
%%
%%------------------------------------------------------------

oe_create() ->
    corba:create(?MODULE, "IDL:JobService/Submitable:1.0").

oe_create_link() ->
    corba:create_link(?MODULE, "IDL:JobService/Submitable:1.0").

oe_create(Env) ->
    corba:create(?MODULE, "IDL:JobService/Submitable:1.0", Env).

oe_create_link(Env) ->
    corba:create_link(?MODULE, "IDL:JobService/Submitable:1.0", Env).

oe_create(Env, RegName) ->
    corba:create(?MODULE, "IDL:JobService/Submitable:1.0", Env, RegName).

oe_create_link(Env, RegName) ->
    corba:create_link(?MODULE, "IDL:JobService/Submitable:1.0", Env, RegName).

%%------------------------------------------------------------
%%
%% Init & terminate functions.
%%
%%------------------------------------------------------------

init(Env) ->
%% Call to implementation init
    corba:handle_init('JobService_Submitable_impl', Env).

terminate(Reason, State) ->
    corba:handle_terminate('JobService_Submitable_impl', Reason, State).


%%%% Operation: submit_job
%% 
%%   Returns: RetVal
%%   Raises:  JobService::HandlerNotRegistered
%%
handle_call({_, OE_Context, submit_job, [JobCtx]}, _, OE_State) ->
  corba:handle_call('JobService_Submitable_impl', submit_job, [JobCtx], OE_State, OE_Context, false, false);

%%%% Operation: '_get_hname'
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, '_get_hname', []}, _, OE_State) ->
  corba:handle_call('JobService_Submitable_impl', '_get_hname', [], OE_State, OE_Context, false, false);

%%%% Operation: '_set_hname'
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, '_set_hname', [OE_Value]}, _, OE_State) ->
  corba:handle_call('JobService_Submitable_impl', '_set_hname', [OE_Value], OE_State, OE_Context, false, false);



%%%% Standard gen_server call handle
%%
handle_call(stop, _, State) ->
    {stop, normal, ok, State};

handle_call(_, _, State) ->
    {reply, catch corba:raise(#'BAD_OPERATION'{minor=1163001857, completion_status='COMPLETED_NO'}), State}.


%%%% Standard gen_server cast handle
%%
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.


%%%% Standard gen_server handles
%%
handle_info(_, State) ->
    {noreply, State}.


code_change(OldVsn, State, Extra) ->
    corba:handle_code_change('JobService_Submitable_impl', OldVsn, State, Extra).

