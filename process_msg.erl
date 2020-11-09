-module(process_msg).
-export([bar/0, bar/1, baz/0]).

bar() ->
  timer:sleep(500),
  io:format("bar started~n"),
  io:format("bar working~n"),
  io:format("bar finished~n").

bar(Pid) ->
  Pid ! "bar started~n",
  Pid ! "bar working~n",
  Pid ! "bar finished~n".

baz() ->
  receive
    stop ->
      io:format("stopped~n");
    Msg ->
      io:format("got: ~s~n",[Msg]),
      baz()
  end.

% spawn(process_msg, bar, []).
% spawn(process_msg, bar, [self()]).
% flush().
% Baz = spawn(process_msg, baz, []).
% Baz ! hello.
% Baz ! stop.
% Baz ! hello.


