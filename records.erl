-module(records).
-compile(export_all).
-include("records.hrl").
-record(robot, {name, 
                type=industrial,
                hobbies,
                details=[]}).

-record(user, {id, name, group, age}).
% include shared records


first_robot() ->
    #robot{name="Mechatron",
           type=handmade,
           details=["Moved by a small man inside."]}.


car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.

% records pattern matching
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".


% the whole record bind to variable U
% in order to use guards on its attrs
adult_section(U=#user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.

% updating records
repairman(Rob) ->
    Details = Rob#robot.details,
    NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
    {repaired, NewRob}.

% using sharing records
new_game() ->
    #game{id=1, title="Custom Game", length=9, durability=120}.