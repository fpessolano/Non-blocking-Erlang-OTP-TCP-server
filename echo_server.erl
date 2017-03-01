
% -------------------------------------------------------------------
%
% Copyright (c) 2017 Fralke nv  All Rights Reserved.
%
% -------------------------------------------------------------------
%
% @doc example of generalised OTP compliant TCP echo server
%
% @author             =   "Francesco Pessolano"
% @license            =   "MIT"
% @version            =   "1.0.0"
% @maintainer         =   "Francesco Pessolano"
% @email              =   "francesco@fralke.com"
% @status             =   "release"
% @credits			  =	  "Jesse Farmer - http://20bits.com"
%
% @end

-module(echo_server).

-export([start/0, loop/2]).

%
% Call start to start the TCP server
%
start() ->
	tcpserver_otp_backend:start(?MODULE, 7000, {?MODULE, loop}, []).


%
% echo_server specific core loop and methods
%
loop(Socket, LoopState) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket, LoopState);
        {error, closed} ->
            ok
    end.

% @end 