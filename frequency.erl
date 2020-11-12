% https://stackoverflow.com/questions/6720472/erlang-and-process-flagtrap-exit-true

-module(frequency).
-export([
  allocate/0,
  allocate/1,
  clear/0,
  clear/1,
  deallocate/1,
  get_all/0,
  init/0,
  inject/1,
  inject/2,
  loop/1,
  start/0,
  stop/0
]).

start() ->
  register(frequency, spawn(frequency, init, [])).

%% These are the start functions used to create and initialize the server.

init() ->
  process_flag(trap_exit, true), % want to receive a message if child process terminates
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

%% Hard Coded

get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      frequency:loop(NewFrequencies);

    {request, Pid, {inject, Freqs}} ->
      NewFrequencies = inject(Frequencies, Freqs),
      Pid ! {reply, injected},
      frequency:loop(NewFrequencies);

    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);

    {request, Pid, get_all} ->
      Pid ! {reply, Frequencies},
      frequency:loop(Frequencies);

    {request, Pid, clear} ->
      frequency ! {request, self(), get_all},
      frequency ! {request, self(), get_all},
      frequency ! {request, self(), get_all},
      clear(Pid),
      Pid ! {reply, cleared},
      frequency:loop(Frequencies);

    {request, Pid, stop} ->
      Pid ! {reply, stopped};

    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      frequency:loop(NewFrequencies)
  end.

%% Functional interface

allocate(Timeout) ->
  frequency ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  after Timeout ->
    ok
  end.

allocate() ->
  frequency ! {request, self(), allocate},
  receive
    {reply, Reply} -> Reply
  end.

inject(Freqs) ->
  frequency ! {request, self(), {inject, Freqs}},
  receive
    {reply, Reply} -> Reply
  end.

deallocate(Freq) ->
  frequency ! {request, self(), {deallocate, Freq}},
  receive
    {reply, Reply} -> Reply
  end.

get_all() ->
  frequency ! {request, self(), get_all},
  receive
    {reply, Reply} -> Reply
  end.

clear() ->
  frequency ! {request, self(), clear},
  receive
    {reply, Reply} -> Reply
  end.

clear(Pid) ->
  receive
    Msg ->
      Pid ! {"deleting message: ", Msg},
      clear(Pid)
  after 0 ->
    ok
  end.

stop() ->
  frequency ! {request, self(), stop},
  receive
    {reply, Reply} -> Reply
  end.

%% The Internal Help Functions used to allocate and deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  { {[], Allocated}, {error, no_frequency} };
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  case lists:keymember(Pid, 2, Allocated) of % [{10,<0.80.0>}]
    true ->
      { {[Freq|Free], Allocated}, {error, already_allocated} };
    _ ->
      { {Free, [{Freq, Pid}|Allocated]}, {ok, Freq} }
  end.

inject({Free, Allocated}, Freqs) ->
  {[Freqs|Free] , Allocated}.

deallocate({Free, Allocated}, Freq) ->
  {value, { Freq, Pid} } = lists:keysearch(Freq, 1, Allocated),
  unlink(Pid),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of % avoiding the race condition with deallocation
    {value, {Freq, Pid}} ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated};
    false -> {Free, Allocated}
  end.

% frequency:start().
% frequency ! {request, self(), allocate}.

% or

% Freq = spawn(frequency, init, []).
% Freq ! {request, self(), allocate}.
% flush().
% Freq ! {request, self(), {deallocate, 10}}.
% flush().
% Freq ! {request, self(), allocate}.
% flush().
% Freq ! {request, self(), get_all}.
% flush().

% or

% frequency:start().
% frequency:allocate().
% => {ok,10}
% frequency:deallocate(10).
% => ok
% frequency:allocate().
% frequency:allocate(1000).
% => {ok,10}
% frequency:get_all().
% => {[11,12,13,14,15],[{10,<0.112.0>}]}
% frequency:inject(11).
% frequency:clear().
% flush().
% frequency:stop().
% => stopped

% code:soft_purge(ModuleName).
