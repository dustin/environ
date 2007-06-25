{application, environ,
        [{description, "Environmental monitoring."},
         {vsn, "1.0"},
         {modules, [ environ ]},

         {registered, []},
         {applications, [kernel,stdlib]},
		 {mod, {environ, []}},
		 {included_applications, [temp_listener, lemp_serv, env_alert]}
]}.

