-module(mailbox).
-export([receiver/0, receiver_stop/0, client/0, server/0, loop/1]).

receiver() ->
  timer:sleep(1000),
  receive
    X -> io:format("message: ~w~n",[X])
  end,
  receiver().

receiver_stop() ->
  timer:sleep(1000),
  receive
    stop -> ok;
    X ->
      io:format("message: ~w~n",[X]),
      receiver_stop()
  end.

client() ->
  Pid = spawn(mailbox, server, []),
  Pid ! {second, "This message should be processed after the first one."},
  Pid ! {first, "This message should be processed at the beginnig."}.

server() ->
  receive
    {first, FirstString} -> io:format("got first: ~s~n", [FirstString])
  end,
  receive
    {second, SecondString} -> io:format("got second: ~s~n", [SecondString])
  end.

clear() ->
  receive
    _Msg -> clear()
  after 0 ->
    ok
  end.

loop(N) ->
  receive
    _Msg -> loop(N)
  after 1000 ->
    io:format("~w~n",[N]),
    loop(N+1)
  end.

% register(server, spawn(M,F,[])),
% mailbox:client().




% RPC

% rpc(Pid, Request) ->
%   Tag = erlang:make_ref(),
%   Pid ! {self(), Tag, Request},
%   receive
%     {Tag, Response} -> Response
%   end.

% =>

% RPC split in two

% rpc(Pid, Request) ->
%   Tag = erlang:make_ref(),
%   Pid ! {self(), Tag, Request},
%   Tag.

% wait_response(Tag) ->
%   receive
%     {Tag, Response} -> Response
%   end.

% =>

% Futures

% promise(Pid, Request) ->
%   Tag = erlang:make_ref(),
%   Pid ! {self(), Tag, Request},
%   Tag.

% yield(Tag) ->
%   receive
%     {Tag, Response} -> Response
%   end.

% Tag = mailbox:promise(Pid, fun() -> timer:sleep(5000), ok end).
% Val = mailbox:yield(Tag).

% pmap
% https://erlang.org/doc/programming_examples/list_comprehensions.html

% function pmap(L) ->
%   S = self(),
%   Pids = [do(S, F) || F <- L],
%   [receive {Pid, Val} -> Val end || Pid <- Pids].

% do(Parent, F) ->
%   spawn(fun() ->
%     Parent ! {self(), F()}
%   end).
