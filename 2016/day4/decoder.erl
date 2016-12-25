% Room Decoder Server

-module(decoder).
-behavior(gen_server).
-export([start_link/0,stop/1,add_room/2]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Calls
add_room(Pid, Room) -> gen_server:cast(Pid, {add_room, Room}).
stop(Pid) -> gen_server:call(Pid, stop).

%%% Server Functions
init([]) -> {ok, []}.

handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call(Msg, _From, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_cast({add_room, Room}, State) ->
    % Determine if the room is a decoy
    {ActualChecksum, IsDecoy} = check_is_decoy(Room),
    RoomEntry = {Room, ActualChecksum, IsDecoy},
    io:format("Adding room to DB: ~p~n", [RoomEntry]),

    % Store the room in the database
    {noreply, [ RoomEntry | State ]};
handle_cast(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_info(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

terminate(Reason, _State) -> 
    io:format("Terminating ~p process because: ~p", ?MODULE, Reason),
    ok.

% We don't have any state migration.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Private
check_is_decoy({Segments, _, Checksum}) ->
    Letters = lists:sort(lists:flatten(Segments)),
    Groups = lists:foldr(fun(Elem, AccIn) ->
                    case lists:keysearch(Elem, 1, AccIn) of
                        false ->
                            [ {Elem, 1} | AccIn];
                        {value, {Letter, Count}} ->
                            lists:keyreplace(Elem, 1, AccIn, {Letter, Count + 1})
                    end
                end,
                [],
                Letters),
    Sorted = lists:reverse(lists:keysort(2, Groups)),
    Top5 = lists:sublist(Sorted, 1, 5),
    ActualChecksum = [ X || {X, _} <- Top5 ],
    {ActualChecksum, Checksum =/= ActualChecksum}.
