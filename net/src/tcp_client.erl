%%%-------------------------------------------------------------------
%%% @author fred
%%% @copyright (C) 2016, <JZYX>
%%% @doc
%%%
%%% @end
%%% Created : 15. 六月 2016 11:45
%%%-------------------------------------------------------------------
-module(tcp_client).

-description("tcp_client").
-copyright({jzyx, 'www.jzyx.com'}).
-author({fred, 'wangxingfred@gmail.com'}).
-vsn(1).

%%%===============================INCLUDE================================

%%%================================EXPORT================================
-export([send/0]).

send() ->
    Host = "localhost",
    Port = 12345,
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    gen_tcp:send(Socket, "Some Data"),
    gen_tcp:close(Socket).
