-module(mess_client).
-export([client/2]).

-include("mess_interface.hrl").

client(Server_Node, Name) ->
    {mess_server, Server_Node} ! {logon, self(), Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        {message_to, Name, Message} ->
            io:format("Message: ~p To ~w~n", [Message, Name]),
            {mess_server, Server_Node} ! 
                {message, self(), Name, Message},
            await_result();
        {message_from, Name, Message} ->
            io:format("From: ~w Message: ~p~n", [Name, Message])
    end,
    client(Server_Node).

await_result() ->
    receive
        {abort_client, Why} ->
            io:format("Stop. Reason: ~w~n", [Why]),
            exit(normal);
        {server_reply, What} ->
            io:format("What: ~w~n", [What])
    after 5000 ->
            io:format("no response~n", []),
            exit(timeout)
    end. 

