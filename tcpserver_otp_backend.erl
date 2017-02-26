% -------------------------------------------------------------------
%
% Copyright (c) 2017 Fralke nv  All Rights Reserved.
%
% -------------------------------------------------------------------
%
% @doc example of generalised OTP compliant TCP server general side
%
% @author             =   "Francesco Pessolano"
% @license            =   "MIT"
% @version            =   "0.5.0"
% @maintainer         =   "Francesco Pessolano"
% @email              =   "francesco@xetal.eu"
% @status             =   "in development"
% @credits			  =	  "Jesse Farmer - http://20bits.com"
%
% @end

-module(tcpserver_otp_backend).
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/4]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
		port,					%% port the server is listening to
		server_loop,			%% the loop the current instance is executing
		loop_state,				%% state initialiser or state of the operational server loop
		ip,						%% IP of the machine where the process is running
		server_socket=null		%% socket of the server
		}).

%%
%% Start loop
%% Requires:
%%	Name 		- process NameScope
%%	Port 		- server port
%%	Loop 		- main loop with the server specific code
%%	LoopState 	- the state initialiser
%% It returns:
%%	the server process PID in case of success
%%	{erron, reason for the error} in case of failure
%%
start(Name, Port, Loop, StateInit) ->
case inet:getif() of 
		{ok,[{IP,_,_},_]} -> 
			State = #server_state{port = Port, server_loop = Loop, ip = IP, loop_state = StateInit},
			gen_server:start_link({local, Name}, ?MODULE, State, []);
		_Other ->
			{error, machine_IP_invalid}
	end.

%%
%% gen_server init (selecting on presence of a initialisation function)
%%

init(State = #server_state{port=Port, loop_state = {Module, Function}}) ->
	LoopState = Module:Function(),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, Server_socket} ->
   			process_flag(trap_exit, true),
   			NewState = State#server_state{server_socket = Server_socket, loop_state = LoopState},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end;

init(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
   		{ok, Server_socket} ->
   			process_flag(trap_exit, true),
   			NewState = State#server_state{server_socket = Server_socket},
   			{ok, accept(NewState)};
   		{error, Reason} ->
   			{stop, Reason}
	end.

%%
%% gen_server:cast definition
%%
handle_cast({accepted, _Pid}, State=#server_state{}) ->
	{noreply, accept(State)}.

%%
%% accept_loop and accept are used to initialise a new TCP loop or terminate the server in case of error
%% it uses a gen_server:cast to start the new loop
%%
accept_loop({Server, Server_socket, {Module, Function}, LoopState}) ->
	case gen_tcp:accept(Server_socket) of 
		{ok, Socket} -> 
			%% Let the server spawn a new process and replace this loop to avoid blocking 
			gen_server:cast(Server, {accepted, self()}),
			Module:Function(Socket, LoopState);
		_Other ->
			%% defensive 
			not_ok
	end.
	
accept(State = #server_state{server_socket=Server_socket, server_loop = Loop, loop_state = LoopState}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), Server_socket, Loop, LoopState}]),
	State.

%%
%% gen_server:handle_call definition
%%
handle_call(_Msg, _Caller, State) -> {noreply, State}.

%%
%% gen_server:handle_info definition
%%
handle_info(Msg, Loopdata) -> 
	io:format("Server ~p received in handle_info {~p, ~p}~n", [self(), Msg, Loopdata]),
	{noreply, Loopdata}.

%%
%% gen_server:terminate definition
%%
terminate(_Reason, _Library) -> ok.

%%
%% gen_server:code_change definition
%%
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

% end 