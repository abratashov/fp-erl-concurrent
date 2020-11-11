-module(scenario).
-export([setup/0, stop/1, client/2, loop/2, random_elem/1]).

% Use this module to exercise the behaviour of the
% hardened frequency server.

% Calling setup will launch the server and two clients: alice and bob.

setup() ->
  frequency:start(),
  spawn(?MODULE, client,[alice,[]]),
  spawn(?MODULE, client,[bob,[]]).

% A client, parametrised by its name (optional, but useful instrumentation),
% and the list of frequencies currently allocated to that process. Needed
% to produce calls to deallocate/1 that don't fail.

% Could also
%   - parameterise on the ratio of allocates to deallocates
%   - deal with case when no frequencies available: here a client fails
%   - add stop commands.

client(Id, Freqs) ->
  erlang:process_flag(trap_exit, true),
  loop(Id, Freqs).

loop(Id, Freqs) ->
  receive
    {'EXIT', _, _} ->
      io:format("Frequency server down. Exit.~n"),
      ok;
    stop ->
      io:format("client ~w asked to stop. Exit.~n", [Id]),
      ok
  after 0 ->
    case rand:uniform(2) of
      1 ->
        case frequency:allocate() of
          {ok, Freq} ->
            io:format("Frequency ~w allocated to client ~w.~n", [Freq, Id]),
            timer:sleep(1000),
            loop(Id, [Freq|Freqs]);
          {error, already_allocated} ->
            io:format("Frequency already allocated to client ~w.~n", [Id]),
            timer:sleep(1000),
            loop(Id, Freqs)
        end;
      2 ->
        Len = length(Freqs),
        case Len of
          0 ->
            io:format("No frequencies to deallocate by client ~w.~n", [Id]),
            timer:sleep(1000),
            loop(Id, Freqs);
          _ ->
            Freq = lists:nth(rand:uniform(Len), Freqs),
            frequency:deallocate(Freq),
            io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
            timer:sleep(1000),
            loop(Id, lists:delete(Freq, Freqs))
        end
    end
  end.

% for debugging purposes: chooses a random element of a non-empty list.

random_elem([]) ->
  empty;
random_elem(Xs) ->
  Len = length(Xs),
  lists:nth(rand:uniform(Len), Xs).

kill_server() ->
  exit(whereis(frequency), kill).

stop(Clients) ->
  [Pid ! stop || Pid <- Clients],
  catch frequency:stop(),
  ok.

% c('/home/alex/Projects/abratashov/fp-erl-concurrent/frequency.erl').
% c('/home/alex/Projects/abratashov/fp-erl-concurrent/scenario.erl').

% observer:start().
% scenario:setup().
% scenario:stop([<0.2711.0>]). % ServerPid
