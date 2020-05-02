{application, 'messenger',
  [{desciption, 'sample messenger app'},
   {vsn, '1,0'},
   {modules, [mess, mess_server, mess_client, user_interface]},
   {registered, {mess_server, mess_client}}
  ]}.