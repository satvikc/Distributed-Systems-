-module(processes).
-export([start/2,quorum/2,process/0,executeCritical/0,lockSharedMemory/2,waitForResponse/3]).
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
    io:format("Process ~p ~n",[self()]),
    receive
        {quorum,Quorum} ->
            List = Quorum,
            io:format("Quorum List for process ~p is ~p ~n",[self(),List]),
            lockSharedMemory(List,{none,none})
    end.

%% Performs the mutual exclusion using maekawa algorithm.
lockSharedMemory(Quorum,State) ->
    %% Send request to all processes in its quorum
    Clock = os:timestamp(),
    lists:map(fun(Pid) ->
                      Pid ! {self(),request,Clock}
              end, Quorum),
    io:format("Request send to all ~n"),
    Responses = lists:map(fun(X) -> {X,none} end,Quorum),
    waitForResponse(State,[],orddict:from_list(Responses)).

waitForResponse(State,Queue,Responses) ->
%%    io:format("~p > Current State ~p Queue ~p and Responses ~p ~n",[self(),State,Queue,Responses]),
    receive
        {From,request,TS} ->
            io:format("~p > request from ~p timestamp is ~p ~n",[self(),From,TS]),
            case State of
                {_,none} ->
                    [{TimeStamp,Pid}|T] = lists:sort([{TS,From}|Queue]),
                    Pid ! {self(),reply},
                    io:format("~p > reply sent to ~p ~n",[self(),Pid]),
                    waitForResponse({TimeStamp,Pid},T,Responses);
                {T,G} ->
                    if {TS,From} < {T,G} ->
                            G ! {self(),inquire},
                            io:format("~p > inquire sent to ~p ~n",[self(),G]);
                       true ->
                            From ! {self(),failed},
                            io:format("~p > failed sent to ~p ~n",[self(),From])
                    end,
                    waitForResponse(State,lists:sort([{TS,From}|Queue]),Responses)
                end;
        {From,reply} ->
            io:format("~p > reply from ~p ~n",[self(),From]),
            NewResponses = orddict:store(From,reply,Responses),
            case allHave(NewResponses,reply) of
               true ->
                    io:format("~p > Entering critical section ~n",[self()]),
                    executeCritical(),
                    io:format("~p > Exiting critical section ~n",[self()]),
                    orddict:map(fun(F,_) -> F ! {self(),release} end,NewResponses),
                    waitForResponse(State,Queue,NewResponses);
               false -> waitForResponse(State,Queue,NewResponses)
            end;
        {From,inquire} ->
            io:format("~p > inquire from ~p ~n",[self(),From]),
            case (anyHave(Responses,failed) or anyHave(Responses,yield)) of
                true ->
                    From ! {self(),yield},
                    io:format("~p > yield sent to ~p ~n",[self(),From]),
                    NewResponses = orddict:store(From,yield,Responses),
                    waitForResponse(State,Queue,NewResponses);
                false ->
                    nothing
            end;
        {From,failed} ->
            io:format("~p > failed from ~p ~n",[self(),From]),
            NewResponses = orddict:store(From,failed,Responses),
            waitForResponse(State,Queue,NewResponses);
        {From,yield} ->
            io:format("~p > yield from ~p ~n",[self(),From]),
            {TS,FR} = State,
            if FR==From ->
                    [{TimeStamp,Pid}|T] = lists:sort([{TS,From}|Queue]),
                    io:format("~p > reply sent to ~p ~n",[self(),Pid]),
                    Pid ! {self(),reply},
                    waitForResponse({TimeStamp,Pid},T,Responses);
               true -> io:format("Illegal yield request from ~p while current state is ~p ~n",[From,State])
            end;
        {From,release} ->
            io:format("~p > release from ~p ~n",[self(),From]),
            {_,FR} = State,
            if FR==From ->
                    case Queue of
                        [{TimeStamp,Pid}|T] ->
                            io:format("~p > reply sent to ~p ~n",[self(),Pid]),
                            Pid ! {self(),reply},
                            waitForResponse({TimeStamp,Pid},T,Responses);
                        [] -> nothing
                    end;
               true -> io:format("Illegal release request from ~p while current state is ~p ~n",[From,State])
            end
    end.





%% Finding the quorum set using the square method.
%% If length of list is not a square then 3rd and 4th codition of quorum set will not be satisfied.
quorum(P,N) -> M = ceiling(sqrt(N)),
               {I,J} = toIJ(P,M),
               Row = lists:map(fun(X) -> fromIJ(I,X,M) end,lists:seq(1,M)),
               Column = lists:map(fun(X) -> fromIJ(X,J,M) end,lists:seq(1,M)),
               Elements = Row ++ Column -- [P],
               lists:filter(fun(X) -> X =< N end,Elements).


%% Critical section
executeCritical() ->
    random:seed(erlang:now()),
    MyGuess = random:uniform(100),
    shared_memory ! {self(),open},
    shared_memory ! {self(),read},
    receive
        V ->
            io:format("CRITICAL ~p > Shared mem = ~p while my guess = ~p ~n",[self(),V,MyGuess]),
            if V == MyGuess ->
                    shared_memory ! {self(),write,random:uniform(100)},
                    shared_memory ! {self(),close},
                    io:format("CRITICAL ~p > I won 1000 Rs. Lets go to party ~n",[self()]);
               true ->
                    shared_memory ! {self(),close},
                    io:format("CRITICAL ~p > I lost. :( ~n",[self()])
            end
    end.


%%%%%%%%%%%%%%%%%
%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%

%% For orddict, check is anyvalue is equal to given value
anyHave(Dict,Value) ->
    OrdDict = orddict:filter(fun(_,V) -> V == Value end,Dict),
    orddict:size(OrdDict) > 0.

%% For orddict, check is all the values are equal to given value
allHave(Dict,Value) ->
    OrdDict = orddict:filter(fun(_,V) -> V == Value end,Dict),
    orddict:size(OrdDict) == orddict:size(Dict).

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
