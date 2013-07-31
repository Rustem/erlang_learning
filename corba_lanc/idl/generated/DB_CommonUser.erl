%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: DB_CommonUser
%% Source: idl/DB.idl
%% IC vsn: 4.2.27
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('DB_CommonUser').
-ic_compiled("4_2_27").


%% Interface functions
-export([lookup/2, lookup/3]).

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



%%%% Operation: lookup
%% 
%%   Returns: RetVal
%%
lookup(OE_THIS, ENo) ->
    corba:call(OE_THIS, lookup, [ENo], ?MODULE).

lookup(OE_THIS, OE_Options, ENo) ->
    corba:call(OE_THIS, lookup, [ENo], ?MODULE, OE_Options).

%%------------------------------------------------------------
%%
%% Inherited Interfaces
%%
%%------------------------------------------------------------
oe_is_a("IDL:DB/CommonUser:1.0") -> true;
oe_is_a(_) -> false.

%%------------------------------------------------------------
%%
%% Interface TypeCode
%%
%%------------------------------------------------------------
oe_tc(lookup) -> 
	{{tk_struct,"IDL:DB/employee:1.0","employee",
                    [{"No",tk_ulong},
                     {"Name",{tk_string,0}},
                     {"Address",{tk_string,0}},
                     {"Dpt",
                      {tk_enum,"IDL:DB/Department:1.0","Department",
                               ["Department1","Department2"]}}]},
         [tk_ulong],
         []};
oe_tc(_) -> undefined.

oe_get_interface() -> 
	[{"lookup", oe_tc(lookup)}].




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
    "IDL:DB/CommonUser:1.0".


%%------------------------------------------------------------
%%
%% Object creation functions.
%%
%%------------------------------------------------------------

oe_create() ->
    corba:create(?MODULE, "IDL:DB/CommonUser:1.0").

oe_create_link() ->
    corba:create_link(?MODULE, "IDL:DB/CommonUser:1.0").

oe_create(Env) ->
    corba:create(?MODULE, "IDL:DB/CommonUser:1.0", Env).

oe_create_link(Env) ->
    corba:create_link(?MODULE, "IDL:DB/CommonUser:1.0", Env).

oe_create(Env, RegName) ->
    corba:create(?MODULE, "IDL:DB/CommonUser:1.0", Env, RegName).

oe_create_link(Env, RegName) ->
    corba:create_link(?MODULE, "IDL:DB/CommonUser:1.0", Env, RegName).

%%------------------------------------------------------------
%%
%% Init & terminate functions.
%%
%%------------------------------------------------------------

init(Env) ->
%% Call to implementation init
    corba:handle_init('DB_CommonUser_impl', Env).

terminate(Reason, State) ->
    corba:handle_terminate('DB_CommonUser_impl', Reason, State).


%%%% Operation: lookup
%% 
%%   Returns: RetVal
%%
handle_call({_, OE_Context, lookup, [ENo]}, _, OE_State) ->
  corba:handle_call('DB_CommonUser_impl', lookup, [ENo], OE_State, OE_Context, false, false);



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
    corba:handle_code_change('DB_CommonUser_impl', OldVsn, State, Extra).
