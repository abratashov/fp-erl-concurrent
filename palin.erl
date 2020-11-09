-module(palin).
-export([
  palin/1,
  nopunct/1,
  palindrome/1,
  nocaps/1,
  reverse/1,
  shunt/2,
  server/1
]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) -> palin(nocaps(nopunct(Xs))).

nopunct([]) -> [];
nopunct([X|Xs]) ->
  case lists:member(X, ".,\ ;:\t\n\'\"") of
    true ->
      nopunct(Xs);
    false ->
      [ X | nopunct(Xs) ]
  end.

nocaps([]) -> [];
nocaps([X|Xs]) -> [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
  case $A =< X andalso X =< $Z of
    true -> X+32;
    false -> X
  end.

% literal palindrome

palin(Xs) -> Xs == reverse(Xs).

reverse(Xs) -> shunt(Xs,[]).

shunt([],Ys) -> Ys;
shunt([X|Xs],Ys) -> shunt(Xs,[X|Ys]).

server(SenderId) ->
  receive
    stop -> stop;

    {check, Msg} ->
      case palin:palindrome(Msg) of
        true ->
          Response = {result, Msg ++ " is a palindrome"},
          SenderId ! Response,
          self() ! Response;

        false ->
          Response = {result, Msg ++ " isn't a palindrome"},
          SenderId ! Response,
          self() ! Response
      end;

    {result, Result} ->
      io:format("got result: ~s~n", [Result])

    % after Time ->
    %   TimeoutActions
  end,
  server(SenderId).

% client
% Self = self(), Sid = spawn(palin, server, [Self]), Sid ! {check, "Asddsa"}.
% flush().

% register(SidName, Sid). ?
% Pid = whereis(SidName). ?
