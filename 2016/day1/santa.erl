-module(santa).
-behavior(gen_server).
-export([start_link/0,move/3,stop/1,where_am_i/1,get_hq/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Calls
move(Pid, Turn, Distance) -> gen_server:cast(Pid, {move, Turn, Distance}).
where_am_i(Pid) -> gen_server:call(Pid, whereami).
get_hq(Pid) -> gen_server:call(Pid, get_hq).
stop(Pid) -> gen_server:call(Pid, terminate).

%%% Server Functions
init([]) -> {ok, {north, {0, 0}, [], notfound}}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call(whereami, _From, {Direction, Position, Visited, HQ}) ->
    {reply, {Direction, Position}, {Direction, Position, Visited, HQ}};

handle_call(get_hq, _From, {Direction, Position, Visited, HQ}) ->
    {reply, HQ, {Direction, Position, Visited, HQ}}.

handle_cast({move, Turn, Distance}, {Direction, Position, Visited, HQ}) ->
    NewDirection = turn(Direction, Turn),
    {NewVisited, NewHQ, NewPosition} = walk(NewDirection, Distance, Position, Visited, HQ),
    {noreply, {NewDirection, NewPosition, NewVisited, NewHQ}}.

visit({X, Y}, Visited, notfound) ->
    NewHQ = case lists:member({X, Y}, Visited) of
        true -> {X, Y};
        false -> notfound
    end,
    NewVisited = [ {X, Y} | Visited ],
    {NewVisited, NewHQ};

visit(_, Visited, HQ) -> {Visited, HQ}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    % We don't have any state migration.
    {ok, State}.

%%% Private
turn(north, left) -> west;
turn(west, left) -> south;
turn(south, left) -> east;
turn(east, left) -> north;
turn(north, right) -> east;
turn(east, right) -> south;
turn(south, right) -> west;
turn(west, right) -> north.

walk(north, Distance, Pos, Visited, HQ) -> move({0, 1}, Pos, Distance, Visited, HQ);
walk(south, Distance, Pos, Visited, HQ) -> move({0, -1}, Pos, Distance, Visited, HQ);
walk(west, Distance, Pos, Visited, HQ) -> move({-1, 0}, Pos, Distance, Visited, HQ);
walk(east, Distance, Pos, Visited, HQ) -> move({1, 0}, Pos, Distance, Visited, HQ).

move(_, {X, Y}, 0, Visited, HQ) -> {Visited, HQ, {X, Y}};
move({DX, DY}, {X, Y}, Distance, Visited, HQ) ->
    {NewX, NewY} = {X + DX, Y + DY},
    {NewVisited, NewHQ} = visit({NewX, NewY}, Visited, HQ),
    move({DX, DY}, {NewX, NewY}, Distance - 1, NewVisited, NewHQ).

