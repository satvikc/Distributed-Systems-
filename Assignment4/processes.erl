-module(processes).
-export([start/2,quorum/2,process/0]).
-import(math,[sqrt/1]).

start(0,List) ->
    N = length(List),
    IndexedList = lists:zip(lists:seq(1,N),List),
    lists:map(fun({I,Pid}) ->
                      Send = {quorum,mapIndex(quorum(I,N),List)},
                      Pid ! Send
                      end, IndexedList),
    io:format("Started Processes are ~p ~n",[List]);
start(N,List) ->
    Pid = spawn(processes,process,[]),
    start(N-1,[Pid|List]).

%% Actual process which access the shared memory using maekawa algorithm.
process() ->
    receive
        {quorum,List} ->
            io:format("Quorum List for process ~p is ~p ~n",[self(),List]),
            lockSharedMemory(List,0,{none,none}),
    end.

%% Performs the mutual exclusion using maekawa algorithm.
lockSharedMemory(Quorum,Clock,State) ->
    %% Send request to all processes in its quorum
    lists:map(fun(Pid) ->
                      Pid ! {self(),request,Clock}
              end, Quorum),
    waitForResponse(Quorum,Clock,State).

waitForResponse(Quorum,Clock,State) ->
    receive
        {From,request,TS} ->
            case State of
                {none,_} ->
                    From ! {self(),reply},
                    waitForResponse(Quorum,Clock+1,{From,TS});
                {G,T} ->
                    if {TS,From} < {T,G} ->
                            G ! {self(),inquire};
                       true ->
                            G ! {self(),failed}
                    end,
                    waitForResponse(Quorum,Clock+1,State);



%% Finding the quorum set using the square method.
%% If length of list is not a square then 3rd and 4th codition of quorum set will not be satisfied.
quorum(P,N) -> M = ceiling(sqrt(N)),
               {I,J} = toIJ(P,M),
               Row = lists:map(fun(X) -> fromIJ(I,X,M) end,lists:seq(1,M)),
               Column = lists:map(fun(X) -> fromIJ(X,J,M) end,lists:seq(1,M)),
               Elements = Row ++ Column -- [P],
               lists:filter(fun(X) -> X =< N end,Elements).




%%%%%%%%%%%%%%%%%
%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%

%% Returns ceiling of a floating point number.
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Helper for calculating quorums
fromIJ(I,J,M) -> (I-1)*M + J.
toIJ(P,M) ->
    J = ((P-1) rem M) + 1,
    I = ((P-1) div M) + 1,
    {I,J}.

%% Helper for working with lists
mapIndex([],_) -> [];
mapIndex([I|Rest],List) ->
    [lists:nth(I,List) | mapIndex(Rest,List)].
