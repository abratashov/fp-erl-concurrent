-module(gfrequency).
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
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

get_frequencies() -> [10,11,12,13,14,15].

handle_call(allocate, Pid, Frequencies) ->
  {NewFrequencies, Reply} = allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies};

handle_call({inject, Freqs}, _From, Frequencies) ->
  NewFrequencies = inject(Frequencies, Freqs),
  {reply, NewFrequencies, NewFrequencies};

% handle_call({deallocate, Freq}, _From, Frequencies) ->  % sync call
%   NewFrequencies = deallocate(Frequencies, Freq),
%   {reply, NewFrequencies, NewFrequencies};

handle_call(get_all, _From, Frequencies) ->
  {reply, Frequencies, Frequencies}.

handle_cast({deallocate, Freq}, Frequencies) -> % async call
  NewFrequencies = deallocate(Frequencies, Freq),
  {noreply, NewFrequencies}.

% default implementations

handle_info(_Info, Frequencies) ->
  {noreply, Frequencies}.

terminate(_Reason, _Frequencies) ->
  ok.

code_change(_OldVsn, Frequencies, _Extra) ->
  {ok, Frequencies}.





% Client requests

allocate() ->
  gen_server:call(?MODULE, allocate).

inject(Freqs) ->
  gen_server:call(?MODULE, {inject, Freqs}).

deallocate(Freq) ->
  gen_server:cast(?MODULE, {deallocate, Freq}).

get_all() ->
  gen_server:call(?MODULE, get_all).

stop() -> % ERROR ?
  gen_server:stop(?MODULE).





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

inject({Free, Allocated}, Freqs) ->
  {[Freqs|Free] , Allocated}.

deallocate({Free, Allocated}, Freq) ->
  {value, { Freq, Pid} } = lists:keysearch(Freq, 1, Allocated),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free], NewAllocated}.

exited({Free, Allocated}, Pid) ->
  case lists:keysearch(Pid, 2, Allocated) of % avoiding the race condition with deallocation
    {value, {Freq, Pid}} ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated};
    false -> {Free, Allocated}
  end.





% c('/home/alex/Projects/abratashov/fp-erl-concurrent/otp/gfrequency.erl').
% gfrequency:start_link().
% gfrequency:allocate().
% gfrequency:deallocate(10).
% gfrequency:allocate().
% gfrequency:get_all().
% gfrequency:inject(11).
% gfrequency:stop().
