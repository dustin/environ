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
				{max_ttl_age, 300},
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
					{startup_alert_recipients, [
						"dustin@spy.net"
					]},
					{mail_server, "mail.west.spy.net"},
					{mail_sender, "dustin+tempalert@spy.net"}
				]}
]}.

