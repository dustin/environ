% arch-tag: F8BEFD9C-A24E-11D8-913D-000A957659CC

{application, env_alert,
        [{description, "Environmental alerts."},
         {vsn, "1.0"},
         {modules, [ env_alert_app, env_alert, env_alert_handler ]},

         {registered, []},
         {applications, [kernel,stdlib,temp_listener,smtp_client]},
		 {mod, {env_alert_app, []}},
		 {env, [
		 		{min_alert_interval, 3600},
				{max_ttl_ages, [
					{"--default--", 600},
					% A week and an hour for my keychain thermometer
					{"2183110000C034BB", 612000}
				]},
				{ranges, [
					{"machineroom", {13, 27}},
					{"bedroom", {8, 31}},
					{"livingroom", {8, 30}},
					{"guestroom", {8, 30}},
					{"purple_mb", {1, 80}},
					{"purple_cpu", {1, 80}},
					{"purple_chip", {1, 80}},
					{"--default--", {-50,50}}
				]},
				{notifications, [
					"dustin@spy.net", "noelani@spy.net",
					"dsallings@2wire.com"
				]},
				{startup_alert_recipients, [
					"dustin@spy.net", "dsallings@2wire.com"
				]},
				{mail_server, "localhost"},
				{mail_sender, "<dustin+tempalert@spy.net>"}
			]}
]}.

