-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    % io:format("server:do_join(...): IMPLEMENT ME~n"),
    % State.
	%3.2.4
	% see if the chatroom exists
	Chatrooms = maps:keys(State#serv_st.chatrooms),
	case lists:member(ChatName, Chatrooms) of 
		true -> 
			ChatRoomPID = maps:get(ChatName, State#serv_st.chatrooms),
			ClientNick = maps:get(ClientPID, State#serv_st.nicks),
			ChatRoomPID!{self(), Ref, register, ClientPID, ClientNick},
			NewReg = lists:append([ClientPID], maps:get(ChatName, State#serv_st.registrations)),
			NewRegistrationMap = maps:update(ChatName, NewReg, State#serv_st.registrations),
			%3.2.7
			State#serv_st {registrations = NewRegistrationMap}
		false -> 
			%spawn chatroom and add to list 
			NewRoom = spawn(chatroom, start_chatroom, [ChatName]),
			% add to chatroom list
			NewChatroomMap = maps:put(ChatName, NewRoom, State#serv_st.chatrooms),
			% add the first client
			NewRegistrationMap = maps:put(ChatName,[ClientPID], State#serv_st.registrations)
			% look up the cliets nickname from server's serv_st record 3.2.5
			ClientNick = maps:get(ClientPID, State#serv_st.nicks),
			%server tells chatroom that client is joining chatroom
			%get PID from the Chat name map 
			ChatRoomPID = maps:get(ChatName, NewChatroomMap),
			%send register message
			ChatRoomPID!{self(), Ref, register, ClientPID, ClientNick},
			% TODO: may have to add nicks at the begining if interp yells at us 
			% send update to the newly added registration and chatroom
			%3.2.7
			State#serv_st {registrations = NewRegistrationMap, chatrooms = NewChatroomMap}
	end.
				
%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    % io:format("server:do_leave(...): IMPLEMENT ME~n"),
    % State.
	%3.3.4 server looks up chatroom pid from server serv_st
	ChatroomPID = maps:get(ChatName, State#serv_st.chatrooms),
	%3.3.5 server removes the client from local record of chatroom registrations
	NewReg = lists:delete(ClientPID, maps:get(ChatName, State#serv_st.registrations)),
	NewRegistrationMap = maps:update(ChatName, NewReg, State#serv_st.registrations),
	%3.3.6 server sends message to the chatroom
	ChatroomPID!{self(), Ref, unregister, ClientPID},
	%% TODO: the chatroom will remove the client from its record of registered clients
	%3.3.7 the server will then send the message  {self(), Ref, ack_leave} to client
	ClientPID!{self(), Ref, ack_leave},
	State#serv_st {registrations = NewRegistrationMap}


%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    % io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
    % State.
	%3.5.4 server needs to check if the new nickname is already used
	% check 
	Nicknames = maps:values(State#serv_st.nicks),
	case lists:member(NewNick, Nicknames) of
		true -> 
			%3.5.4 if it already exists throw an error back to the client
			ClientPID!{self(), Ref, err_nick_used}
		false -> 
			% 3.5.5 point client PID to new nickname
			NewNicknames = maps:update(ClientPID, NewNick, State#serv_st.nicks),
			% search through client PIDs in registrations to find chatroom names
			% then send message to each chatroom in get(chatroom, pid, chatrooms) 3.5.6
			ClientsChatrooms = maps:filter(fun(PIDs)-> lists:member(ClientPID, PIDs) end, State#serv_st.registrations),
			ChatroomNames = maps:keys(ClientsChatooms),
			ChatroomsPIDs = maps:filter(fun(Names)-> lists:member(Names, ChatroomNames)) end, State#serv_st.chatrooms),
			%% for each chatroom send message to update nick 3.5.6
			maps:foreach(fun(ChatroomPID) -> ChatroomPID!{self(), Ref, update_nick, ClientPID, NewNick} end), ChatroomsPIDs),
			% 3.5.7 send message to client
			ClientPID!{self(), Ref, ok_nick},
			State#serv_st {nicks = NewNicknames}
	end. 

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    % io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    % State.
	%3.7.3 
	%remove client from nicknames, 
	NewNickMap = maps:remove(ClientPid, Server#serv_st.nicks),
	%%tell each chatroom to which the client is registered that the eclient is leaving
	ClientsChatrooms = maps:filter(fun(PIDs)-> lists:member(ClientPID, PIDs) end, State#serv_st.registrations),
	ChatroomNames = maps:keys(ClientsChatooms),
	ChatroomsPIDs = maps:filter(fun(Names)-> lists:member(Names, ChatroomNames)) end, State#serv_st.chatrooms),
	maps:foreach(fun(ChatroomPID) -> ChatroomPID!{self(), Ref, unregister, ClientPID}, end), ChatroomsPIDs),
	%% remove client from server's copy of chat registrations
	NewRegistrationMap = maps:map(fun(Chatrooms) -> lists:delete(ClientPID, Chatrooms) end, State#serv_st.registrations),
	%3.7.4 server sends back to client
	ClientPID!{self(), Ref, ack_quit},
	% Update the State to confirm the change 
	State#serv_st {nicks = NewNickMap, registrations = NewRegistrationMap}.
	