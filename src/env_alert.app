{application, env_alert,
        [{description, "Environmental alerts."},
         {vsn, "1.0"},
         {modules, [ env_alert_app, env_alert, env_alert_handler ]},

         {registered, []},
         {applications, [kernel,stdlib,temp_listener,smtp_client,mnesia]},
		 {mod, {env_alert_app, []}},
		 {env, [
		 		{min_alert_interval, 3600},
				{max_ttl_ages, [
					{"--default--", 600},
					% A week and an hour for my keychain thermometer
					{"2183110000C034BB", 612000}
				]},
				{ranges, [
					{"machineroom", {5, 27}},
					{"bedroom", {4, 31}},
					{"livingroom", {5, 30}},
					{"guestroom", {5, 30}},
					{"purple_mb", {1, 80}},
					{"purple_cpu", {1, 80}},
					{"purple_chip", {1, 80}},
					{"--default--", {-50,50}}
				]},
				{notifications, [
					"dustin@spy.net", "noelani@spy.net"
				]},
				{xmpp_notifications, [
					"tempalert"
				]},
				{startup_alert_recipients, [
					"dustin@spy.net"
				]},
				{mail_server, "localhost"},
				{mail_sender, "<dustin+tempalert@spy.net>"}
			]}
]}.

