% arch-tag: 7087B7CA-9DA2-11D8-A84E-000A957659CC

{application, environ,
        [{description, "Environmental monitoring."},
         {vsn, "1.0"},
         {modules, [ environ, environ_sup, lemp_handler,
		 	lemp_serv, temp_listener, environ_mailer, environ_utilities ]},

         {registered, [temp_listener, temp_listener_events]},
         {applications, [kernel,stdlib,smtp_client]},
		 {mod, {environ, []}},
		 {env, [
		 		{min_alert_interval, 3600},
				{max_ttl_age, 900},
		 		{therms, [
						{"1081841E000000DF", "bedroom"},
						{"10258D2A000000EA", "backyard"},
						{"10C8892A00000096", "livingroom"},
						{"101D8A2A000000F7", "newmachineroom"},
						{"10E8C214000000E4", "garage"},
						{"1013A51E00000035", "guestroom"}
						% {"2183110000C034BB", "dustinkeychain"},
						% {"21508400004025AF", "kitchen"},
					]},
					{ranges, [
						{"newmachineroom", {13, 26}},
						{"bedroom", {10, 31}},
						{"livingroom", {13, 30}},
						{"guestroom", {8, 30}},
						{"--default--", {-50,50}}
					]},
					{notifications, [
						"dustin@spy.net"
					]},
					{mail_server, "mail.west.spy.net"},
					{mail_sender, "dustin+tempalert@spy.net"}
				]}
]}.

