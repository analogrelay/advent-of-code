% Keypad Server

-module(keypad).
-behavior(gen_server).
-export([start_link/1,stop/1,move/2,getkey/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link(Mode) -> gen_server:start_link(?MODULE, [Mode], []).

%% Calls
move(Pid, Direction) -> gen_server:cast(Pid, {move, Direction}).
stop(Pid) -> gen_server:call(Pid, stop).
getkey(Pid) -> gen_server:call(Pid, getkey).

%%% Server Functions
init([Mode]) -> {ok, {5, Mode}}.

handle_call(getkey, _From, {Pos, Mode}) -> {reply, Pos, {Pos, Mode}};
handle_call(stop, _From, {Pos, Mode}) -> {stop, normal, ok, {Pos, Mode}};
handle_call(Msg, _From, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_cast({move, Direction}, {Pos, Mode}) -> {noreply, {next_key(Mode, Pos, Direction), Mode}};
handle_cast(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_info(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

terminate(normal, _State) -> ok.

% We don't have any state migration.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Private

% Performs the move. A bit cheaty, but it's how I'd legit do it... lookup tables ftw. There's probably an algorithm but why waste the effort :)
next_key(standard, 1, up) -> 1;
next_key(standard, 1, right) -> 2;
next_key(standard, 1, down) -> 4;
next_key(standard, 1, left) -> 1;
next_key(standard, 2, up) -> 2;
next_key(standard, 2, right) -> 3;
next_key(standard, 2, down) -> 5;
next_key(standard, 2, left) -> 1;
next_key(standard, 3, up) -> 3;
next_key(standard, 3, right) -> 3;
next_key(standard, 3, down) -> 6;
next_key(standard, 3, left) -> 2;
next_key(standard, 4, up) -> 1;
next_key(standard, 4, right) -> 5;
next_key(standard, 4, down) -> 7;
next_key(standard, 4, left) -> 4;
next_key(standard, 5, up) -> 2;
next_key(standard, 5, right) -> 6;
next_key(standard, 5, down) -> 8;
next_key(standard, 5, left) -> 4;
next_key(standard, 6, up) -> 3;
next_key(standard, 6, right) -> 6;
next_key(standard, 6, down) -> 9;
next_key(standard, 6, left) -> 5;
next_key(standard, 7, up) -> 4;
next_key(standard, 7, right) -> 8;
next_key(standard, 7, down) -> 7;
next_key(standard, 7, left) -> 7;
next_key(standard, 8, up) -> 5;
next_key(standard, 8, right) -> 9;
next_key(standard, 8, down) -> 8;
next_key(standard, 8, left) -> 7;
next_key(standard, 9, up) -> 6;
next_key(standard, 9, right) -> 9;
next_key(standard, 9, down) -> 9;
next_key(standard, 9, left) -> 8;

next_key(weird, 1, up) -> 1;
next_key(weird, 1, right) -> 1;
next_key(weird, 1, down) -> 3;
next_key(weird, 1, left) -> 1;

next_key(weird, 2, up) -> 2;
next_key(weird, 2, right) -> 3;
next_key(weird, 2, down) -> 6;
next_key(weird, 2, left) -> 2;

next_key(weird, 3, up) -> 1;
next_key(weird, 3, right) -> 4;
next_key(weird, 3, down) -> 7;
next_key(weird, 3, left) -> 2;

next_key(weird, 4, up) -> 4;
next_key(weird, 4, right) -> 4;
next_key(weird, 4, down) -> 8;
next_key(weird, 4, left) -> 3;

next_key(weird, 5, up) -> 5;
next_key(weird, 5, right) -> 6;
next_key(weird, 5, down) -> 5;
next_key(weird, 5, left) -> 5;

next_key(weird, 6, up) -> 2;
next_key(weird, 6, right) -> 7;
next_key(weird, 6, down) -> a;
next_key(weird, 6, left) -> 5;

next_key(weird, 7, up) -> 3;
next_key(weird, 7, right) -> 8;
next_key(weird, 7, down) -> b;
next_key(weird, 7, left) -> 6;

next_key(weird, 8, up) -> 4;
next_key(weird, 8, right) -> 9;
next_key(weird, 8, down) -> c;
next_key(weird, 8, left) -> 7;

next_key(weird, 9, up) -> 9;
next_key(weird, 9, right) -> 9;
next_key(weird, 9, down) -> 9;
next_key(weird, 9, left) -> 8;

next_key(weird, a, up) -> 6;
next_key(weird, a, right) -> b;
next_key(weird, a, down) -> a;
next_key(weird, a, left) -> a;

next_key(weird, b, up) -> 7;
next_key(weird, b, right) -> c;
next_key(weird, b, down) -> d;
next_key(weird, b, left) -> a;

next_key(weird, c, up) -> 8;
next_key(weird, c, right) -> c;
next_key(weird, c, down) -> c;
next_key(weird, c, left) -> b;

next_key(weird, d, up) -> b;
next_key(weird, d, right) -> d;
next_key(weird, d, down) -> d;
next_key(weird, d, left) -> d.
