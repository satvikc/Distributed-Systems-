-module(shared_mem).
-export([start/0,loop/2]).

%% Starts the shared memory process.
start() -> register(shared_memory,spawn(shared_mem,loop,[none,none])).

%% loop

loop(Holded,State) ->
     receive
         %% Opens the shared memory for reading.
         {From,open} ->
             if Holded == none ->
              io:format("Shared Memory Open request from ~p ~n",[From]),
              loop(From,State);
                true  -> io:format("Illegal shared memory access request from ~p. It is already holded by ~p ~n.",[From,Holded]),
                         loop(Holded,State)
             end;
         %% Writes the Value to the shared Memory. Only the process which opened the shared memory can write.
         {From,write,Value} ->
             if (Holded == From) and (Holded /= none) ->
                     From ! State,
                     loop(Holded,Value);
                true ->
                     io:format("Illegal write request from ~p while shared memory is opened by ~p ~n",[From,Holded]),
                     loop(Holded,State)
             end;
         %% Returns the read value from the shared memory. Any process can read from shared memory.
         {From,read} ->
             From ! State,
             loop(Holded,State);
         %% Closes the shared memory.
         {From,close} ->
             if (Holded == From) and (Holded /= none) ->
                     loop(none,State);
                true -> io:format("Illegal close request from ~p while shared memory is holded by ~p ~n",[From,Holded]),
                        loop(From,State)
             end
     end.
