
% -------------------------------------------------------------------
%
% Copyright (c) 2017 Fralke nv  All Rights Reserved.
%
% -------------------------------------------------------------------
%
% @doc example of generalised OTP compliant TCP server
%		counting the times a message is passed
%
% @author             =   "Francesco Pessolano"
% @license            =   "MIT"
% @version            =   "1.0.0"
% @maintainer         =   "Francesco Pessolano"
% @email              =   "francesco@xetal.eu"
% @status             =   "in development"
% @credits			  =	  "Jesse Farmer - http://20bits.com"
%
% @end

-module(newmsg_server).
-export([start/0, loop/2, newCommand/2, initState/0]).

-record(loop_state, {
		commands
		}).


%
% Call start to start the TCP server
%
start() ->
	tcpserver_otp_backend:start(?MODULE, 7000, {?MODULE, loop}, {?MODULE,initState}).


%
% server specific core loop and methods
% any other TCP server will be defined here
%

% initialise the server state
initState() -> 
	#loop_state{commands = ets:new(commands, [public])}.


%% Core loop
loop(Socket, LoopState = #loop_state{commands = Commands}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
        	A = newCommand(Commands, Data), 
        	gen_tcp:send(Socket, A),
            loop(Socket, LoopState);
        {error, closed} ->
            ok
    end.


%% Internal server specific functions
 newCommand(CommandTableId, Command) ->
 	case ets:lookup(CommandTableId,Command) of 
 		[] ->
 			_A = ets:insert(CommandTableId, {Command, 1}),
 			<<1, "new", "1">>;
 		_ ->
 			A = binary:encode_unsigned(48+ets:update_counter(CommandTableId, Command, 1)),
 			autoPackSendData(A, <<"num">>)
 	end.


%
% Generic data management data
% it includes packing and upcacking of TCP messages  based on the standard
% format {data lenght, data type, data} 
%

autoPackSendData(Data, Type) ->
	Lenght = binary:encode_unsigned(trunc(bit_size(Data) / 8)),
	<< Lenght/binary, Type/binary, Data/binary >>.

unpackReceivedData(Data) ->
	<< Lenght:8/bitstring, Type:8/bitstring, TrueData/bitstring >> = Data,
	[Lenght, Type, TrueData].

% @end 