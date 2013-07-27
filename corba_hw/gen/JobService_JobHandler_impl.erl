%%----------------------------------------------------------------------
%% <LICENSE>
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Module       : JobService_JobHandler_impl.erl
%% 
%% Source       : idl/JobService.idl
%% 
%% Description  : 
%% 
%% Creation date: 2013-07-27
%%
%%----------------------------------------------------------------------
-module('JobService_JobHandler_impl').

-export([handle_job/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%----------------------------------------------------------------------
%% Include Files
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {}).

%%======================================================================
%% API Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : handle_job/2
%% Arguments  : State - term()
%%              JobCtx = #'JobService_job'{Title}
%%              Title = String()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Status
%%              Status = 'success' | 'fail' 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
handle_job(State, JobCtx) ->
	io:format("handling job"),
	timer:sleep(3000),
	io:format("handling job finished"),
	{reply, success, State}.

%%======================================================================
%% Internal Functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Arguments  : Env = term()
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Raises     : -
%% Description: Initiates the server
%%----------------------------------------------------------------------
init(_Env) ->
	{ok, #state{}}.


%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Arguments  : Reason = normal | shutdown | term()
%%              State = term()
%% Returns    : ok
%% Raises     : -
%% Description: Invoked when the object is terminating.
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.


%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Arguments  : OldVsn = undefined | term()
%%              State = NewState = term()
%%              Extra = term()
%% Returns    : {ok, NewState}
%% Raises     : -
%% Description: Invoked when the object should update its internal state
%%              due to code replacement.
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%----------------------------------------------------------------------
%% Function   : handle_info/2
%% Arguments  : Info = normal | shutdown | term()
%%              State = NewState = term()
%% Returns    : {noreply, NewState}          |
%%              {noreply, NewState, Timeout} |
%%              {stop, Reason, NewState}
%% Raises     : -
%% Description: Invoked when, for example, the server traps exits.
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.


