{application, lemp_serv,
	[{description, "Provide a TCP interface for remote temperature monitoring"},
	 {vsn, "1.0"},
	 {modules, [lemp_serv_app, lemp_serv, lemp_handler]},
	 {registered, []},
	 {applications, [kernel,stdlib,temp_listener]},
	 {mod, {lemp_serv_app, []}}
	]}.
