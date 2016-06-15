%%%-------------------------------------------------------------------
%%% @author fred
%%% @copyright (C) 2016, <JZYX>
%%% @doc
%%%
%%% @end
%%% Created : 15. 六月 2016 10:26
%%%-------------------------------------------------------------------
-module(tcp_server).
-description("tcp_server").
-copyright({jzyx, 'www.jzyx.com'}).
-author({fred, 'wangxingfred@gmail.com'}).
-vsn(1).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(HIBERNATE, 1000).

-record(state, {listen_socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    Port = 12345,
    Opts = [
        inet,
        binary,
        {backlog, 256},
        {reuseaddr, true},
        {exit_on_close, false},
        {active, false}
    ],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    %% TODO delete
    io:format("~n------------------~p:~p-----------------~n", [?FILE, ?LINE]),
    io:format("ListenSocket = ~p~n", [ListenSocket]),
    io:format("-----------------------------------------------------------------~n"),
    {ok, #state{listen_socket = ListenSocket}, ?HIBERNATE}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(timeout, State = #state {listen_socket = ListenSocket}) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    R = gen_tcp:recv(Socket, 0),
    %% TODO delete
    io:format("~n------------------~p:~p-----------------~n", [?FILE, ?LINE]),
    io:format("R = ~p~n", [R]),
    io:format("-----------------------------------------------------------------~n"),
    gen_tcp:close(Socket),
    {noreply, State, ?HIBERNATE};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Get the tcp_module, but IPv6 address overrides default IPv4
mod(Address) ->
    case inet_db:tcp_module() of
        inet_tcp when tuple_size(Address) =:= 8 ->
            inet6_tcp;
        Mod ->
            Mod
    end.

%% Get the tcp_module, but option tcp_module|inet|inet6 overrides
mod([{tcp_module,Mod}|_], _Address) ->
    Mod;
mod([inet|_], _Address) ->
    inet_tcp;
mod([inet6|_], _Address) ->
    inet6_tcp;
mod([{ip, Address}|Opts], _) ->
    mod(Opts, Address);
mod([{ifaddr, Address}|Opts], _) ->
    mod(Opts, Address);
mod([_|Opts], Address) ->
    mod(Opts, Address);
mod([], Address) ->
    mod(Address).