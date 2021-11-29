-module(client).

-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
	    {{dummy_target, dummy_response}, State};

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
	%code given
    % io:format("client:do_join(...): IMPLEMENT ME~n"),
    % {{dummy_target, dummy_response}, State}.
	% State#cl_st.con_ch
	Chatrooms = maps:keys(State#cl_st.con_ch),
	% check if already in the chat with the same name, 
	% 3.2.2
	case lists:member(ChatName, Chatrooms) of
		true ->
				whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err},
				{err, State};
		false ->  
				% send "ask to join" message to the server @ ServerPID
				% 3.2.3
				% server pid is stored in whereis(server)
				whereis(server)!{self(), Ref, join,ChatName},
				%% receive connect message
				receive 
					%3.2.8
					{ChatPID, Ref, connect, History} ->
						% update connected chatrooms
						%send{result,self(), Ref, History} SEND IT ALL TO GUI TO WRITE TO THE SCREEN
						%3.2.9
						whereis(list_to_atom(State#cl_st.gui))!{result,self(), Ref, History},
						{History, #cl_st{con_ch = maps:put(ChatName, ChatPID, State#cl_st.con_ch)}}
				end
	end.

%% executes `/leave` protocol from client perspective
do_leave(State, Ref, ChatName) ->
    % io:format("client:do_leave(...): IMPLEMENT ME~n"),
    % {{dummy_target, dummy_response}, State}.
	Chatrooms = maps:keys(State#cl_st.con_ch),
	case lists:member(ChatName,Chatrooms) of 
		true -> 
			%% if found then send leave message 3.3.3
			whereis(server)!{self(), Ref, leave, ChatName},
			%recieve message from server 3.3.7
			receive 
				{_, Ref, ack_leave} -> 
					%3.3.8 client removes the chatroom from list of chatrooms
					UpState = #cl_st{gui = State#cl_st.gui, nick = State#cl_st.nick, con_ch = maps:remove(ChatName, State#cl_st.con_ch)},
					%3.3.9 client sends message back to the GUI
					whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, ok},
					{ack_leave, UpState}
			end;
		false->
			%%if not throw the error to the GUI ----- 3.3.2
			State#cl_st.gui!{result, self(), Ref, err},
			{err, State}
		end. 

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
    % io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    % {{dummy_target, dummy_response}, State}.
	CurrentNick = State#cl_st.nick,
	case (CurrentNick == NewNick) of
		true ->
			%if it is the same client sends message to gui  3.5.2 
			whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err_same},
			{err, State};
		false ->
			% if not the same send nickname to server to update
			%3.5.3
			whereis(server)!{self(), Ref, nick, NewNick},
			receive
				{_ , Ref, err_nick_used} -> 
						%% 3.5.4 take message and pass back to the gui from the server 
						whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, err_nick_used},
						{err_nick_used, #cl_st {gui = State#cl_st.gui, nick = NewNick, con_ch = State#cl_st.con_ch}};
				{_ , Ref, ok_nick} -> 
						%3.5.8 client sends back to gui 
						% update nick locally 
						whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, ok_nick},
						{ok_nick, #cl_st {gui = State#cl_st.gui, nick = NewNick, con_ch = State#cl_st.con_ch}}
			end
	end.

%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
    % io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    % {{dummy_target, dummy_response}, State}.
	%3.6.1.2
	%look up the PID of the chatroom its connected to 
	ChatroomPID= maps:get(ChatName, State#cl_st.con_ch),
	%3.6.1.3 sending client will then send message to the chatroomn
	ChatroomPID!{self(), Ref, message, Message},
	receive
		{ _, Ref, ack_msg} ->
			% message recieved as per 3.6.1.4
			% then send to gui
			io:fwrite("~s",State#cl_st.gui),
			whereis(list_to_atom(State#cl_st.gui))!{result, self(), Ref, {msg_sent, State#cl_st.nick}},
			{ack_msg, State}
	end.
 	
%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
    % io:format("client:do_new_nick(...): IMPLEMENT ME~n"),
    % {{dummy_target, dummy_response}, State}.
	% client sends message to server that we want to quit 3.7.1
	whereis(server)!{self(), Ref, quit},
	receive
		{ _, Ref, ack_quit} ->
			%3.7.5 client must send quit to GUI
			whereis(list_to_atom(State#cl_st.gui))!{self(), Ref, ack_quit}
	end,
	% 3.7.6 the client muyst cleanly exit 
	exit("Goodbye...").

			
