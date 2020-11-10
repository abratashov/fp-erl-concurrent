-module(frequency).
-export([start/0,allocate/0,allocate/1,deallocate/1,stop/0, get_all/0, clear/0, clear/1]).
-export([init/0]).

start() ->
  register(frequency, spawn(frequency, init, [])).

%% These are the start functions used to create and initialize the server.

init() ->
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
      loop(NewFrequencies);

    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);

    {request, Pid, get_all} ->
      Pid ! {reply, Frequencies},
      loop(Frequencies);

    {request, Pid, clear} ->
      frequency ! {request, self(), get_all},
      frequency ! {request, self(), get_all},
      frequency ! {request, self(), get_all},
      clear(Pid),
      Pid ! {reply, cleared},
      loop(Frequencies);

    {request, Pid, stop} ->
      Pid ! {reply, stopped}
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
  case lists:keymember(Pid, 2, Allocated) of % [{10,<0.80.0>}]
    true ->
      { {[Freq|Free], Allocated}, {error, already_allocated} };
    _ ->
      { {Free, [{Freq, Pid}|Allocated]}, {ok, Freq} }
  end.

deallocate({Free, Allocated}, Freq) ->
  case lists:keymember(Freq, 1, Allocated) of % [{10,<0.80.0>}]
    true ->
      NewAllocated=lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free],  NewAllocated};
    _ ->
      {Free, Allocated}
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
% frequency:clear().
% flush().
% frequency:stop().
% => stopped
