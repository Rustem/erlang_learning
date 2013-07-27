%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: JobService_job
%% Source: idl/JobService.idl
%% IC vsn: 4.2.27
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('JobService_job').
-ic_compiled("4_2_27").


-include("JobService.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:JobService/job:1.0","job",
                   [{"title",{tk_string,0}},
                    {"salary",tk_ulong},
                    {"currency",
                     {tk_enum,"IDL:JobService/JobGeoDetails/CurrencyEnum:1.0",
                              "CurrencyEnum",
                              ["kzt","usd","rub","gbp"]}},
                    {"country",
                     {tk_enum,"IDL:JobService/JobGeoDetails/CountryEnum:1.0",
                              "CountryEnum",
                              ["kazakhstan","russia","great_britain","usa"]}},
                    {"reqments",{tk_sequence,{tk_string,146},4}},
                    {"job_details",{tk_sequence,{tk_string,146},4}}]}.

%% returns id
id() -> "IDL:JobService/job:1.0".

%% returns name
name() -> "JobService_job".



