-module(guess_gender).
-compile(export_all).

greet(male, Name) ->
    io:format('Hello, Mr ~s', [Name]);

greet(female, Name) ->
    io:format('Helo, Mrs ~s', [Name]);

greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

%% Guess a gender