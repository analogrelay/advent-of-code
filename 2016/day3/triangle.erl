% Triangle Validator Server

-module(triangle).
-behavior(gen_server).
-export([start_link/0,stop/1,is_triangle/2]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Calls
is_triangle(Pid, Tri) -> gen_server:call(Pid, {is_triangle, Tri}).
stop(Pid) -> gen_server:call(Pid, stop).

%%% Server Functions
init([]) -> {ok, {}}.

handle_call({is_triangle, {A, B, C}}, _From, State) when A + B > C, A + C > B, B + C > A -> {reply, true, State};
handle_call({is_triangle, _}, _From, State) -> {reply, false, State};
handle_call(stop, _From, State) -> {stop, normal, ok, State};
handle_call(Msg, _From, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_cast(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

handle_info(Msg, State) -> io:format("Unknown Message: ~p", [Msg]), {stop, unknown_message, State}.

terminate(normal, _State) -> ok.

% We don't have any state migration.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

