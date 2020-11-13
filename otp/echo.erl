-module(echo).
-compile(export_all).

% Server initializing

start_link() ->
  Pid = spawn_link(?MODULE, init, []),
  register(?MODULE, Pid).

init() ->
  State = 0,
  loop(State).

loop(N) ->
  receive
    {count, From} ->
      From ! N,
      loop(N);

    {{reset, X}, _From} -> loop(X);

    {stop, _From} -> ok;

    {Msg, From} ->
      From ! Msg,
      loop(N+1)
  end.

% Client requests

count() ->
  ?MODULE ! {count, self()},
  receive
    Reply -> Reply
  end.

echo(X) ->
  ?MODULE ! {X, self()},
  receive
    Reply -> Reply
  end.

reset(N) ->
  ?MODULE ! {{reset, N}, self()},
  ok.

stop() ->
  ?MODULE ! {stop, self()},
  ok.

% c('/home/alex/Projects/abratashov/fp-erl-concurrent/otp/echo.erl').
% echo:start_link().
% echo:count().
% echo:echo(msg1).
% echo:echo(msg2).
% echo:count().
% echo:reset(34).
% echo:echo(msg3).
% echo:count().
% echo:stop().

