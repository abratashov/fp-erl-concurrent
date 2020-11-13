-module(gecho).
-behaviour(gen_server).
-compile(export_all).

% Server initializing

start_link() ->
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    [],
    []
  ).

init([]) ->
  {ok, 0}.

handle_call(count, _From, State) ->
  {reply, State, State};

handle_call(Msg, _From, State) ->
  {reply, Msg, State + 1}.

handle_cast({reset, N}, _State) ->
    {noreply, N}.

% handle_info() -> ; % non call/cast msgs
% terminate() -> ; % tidy up on close
% code_change() -> ; % upgrade state


% Client requests

count() ->
  gen_server:call(?MODULE, count). % Sync call

echo(X) ->
  gen_server:call(?MODULE, X). % Sync call

reset(N) ->
  gen_server:cast(?MODULE, {reset, N}). % Async call

stop() ->
  gen_server:stop(?MODULE).


% c('/home/alex/Projects/abratashov/fp-erl-concurrent/otp/gecho.erl').
% echo:start_link().
% echo:count().
% echo:echo(msg1).
% echo:echo(msg2).
% echo:count().
% echo:reset(34).
% echo:echo(msg3).
% echo:count().
% echo:stop().
