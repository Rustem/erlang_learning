%%----------------------------------------------------------------------
%% <LICENSE>
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Module       : JobService_Storeable_impl.erl
%% 
%% Source       : idl/JobService.idl
%% 
%% Description  : 
%% 
%% Creation date: 2013-07-27
%%
%%----------------------------------------------------------------------
-module('JobService_Storeable_impl').

-export([store_job/2]).
-export(['_get_hname'/1, '_set_hname'/2]).

%%----------------------------------------------------------------------
%% Internal Exports
%%----------------------------------------------------------------------
-export([init/1,
         terminate/2,
         code_change/3,
         handle_info/2]).

-define(SERVICE_NAME, "StoreableJobService").

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
%% Function   : store_job/2
%% Arguments  : State - term()
%%              JobCtx = #'JobService_job'{title,salary,currency,country,reqments,job_details}
%%              title = String()
%%              salary = unsigned_Long()
%%              currency = CurrencyEnum
%%              CurrencyEnum = 'kzt' | 'usd' | 'rub' | 'gbp' 
%%              country = CountryEnum
%%              CountryEnum = 'kazakhstan' | 'russia' | 'great_britain' | 'usa' 
%%              reqments = [ reqmentsElem ]
%%              reqmentsElem = String()
%%              job_details = [ job_detailsElem ]
%%              job_detailsElem = String()
%% Returns    : ReturnValue = OE_Reply
%%              OE_Reply = Status
%%              Status = 'success' | 'fail' 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
store_job(State, JobCtx) ->
	{reply, success, State}.

%%----------------------------------------------------------------------
%% Function   : '_get_hname'/1
%% Arguments  : State - term()
%% Returns    : ReturnValue = Hname
%%              Hname = String()
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
'_get_hname'(State) ->
	{reply, ?SERVICE_NAME, State}.

%%----------------------------------------------------------------------
%% Function   : '_set_hname'/2
%% Arguments  : State - term()
%%              Hname = String()
%% Returns    : ReturnValue = ok
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
'_set_hname'(State, Hname) ->
	{reply, ok, State}.

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


