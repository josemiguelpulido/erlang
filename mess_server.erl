-module(mess_server).
-export([start_server/0, server/0]).

-include("mess_interface.hrl").

server() ->                
    process_flag(trap_exit, true),
    server([]).


server(User_List) ->
    
    receive 
        {logon, From, Name} ->
            io:format("processing ~p logon~n", [Name]),
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {EXIT, From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {message, From, ToName, Message} ->
            io:format("message ~p for ~w~n",[Message, ToName]),
            server_transfer(From, ToName, Message, User_List),
            server(User_List)
    end.

start_server() ->
    register(mess_server,
             spawn(mess_server, server, [])).

server_logon(From, Name, User_List) ->
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {abort_client, user_exists_in_another_node},
            User_List;
        false ->
            From ! {server_reply, logged_on},
            link(From),
            [{From, Name} | User_List]
    end.

server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

server_transfer(From, ToName, Message, User_List) ->
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {abort_client, you_are_not_logged_on};
        {value, {_, Name}} ->
            io:format("message ~p for ~w~n",[Message, ToName]),
            server_transfer(From, Name, ToName, Message, User_List)
    end.

server_transfer(From, FromName, ToName, Message, User_List) ->
    case lists:keysearch(ToName, 2, User_List) of
        false ->
            io:format("Process ~w not found in ~w~n",
                      [ToName,User_List]),
            From ! {server_reply, receiver_not_found};
        {value, {To, _}} ->
            io:format("message ~p for ~w from ~w~n",
                      [Message, ToName, FromName]),
            To ! {message_from, FromName, Message},
            From ! {server_reply, message_sent}
    end.

