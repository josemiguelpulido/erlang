
%%% from client to server
-record(logon, {client_pid, client_name}).
-record(message, {client_pid, to_name, message}).

%%% from server to client
-record(abort_client, {message}).
%% messages are: 
%% - user_exists_at_another_node
%% - you_are_not_logged on

-record(server_reply, {message}).
%% messages are: 
%% - logged_on
%% - receiver_not_found
%% - sent

-record(message_from, {from_name, message}).

%%% from shell to client
-record(message_to, {to_name, message}).
