% arch-tag: 7087B7CA-9DA2-11D8-A84E-000A957659CC

{application, environ,
        [{description, "Environmental monitoring."},
         {vsn, "1.0"},
         {modules, [ ]},

         {registered, [temp_listener, temp_listener_events]},
         {applications, [kernel,stdlib]}
]}.

