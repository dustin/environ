{application, temp_listener,
	[{description, "Listen to temperature announcements, send local events"},
	 {vsn, "1.0"},
	 {modules, [temp_listener_app, temp_listener, gen_sup, environ_utilities]},
	 {registered, [temp_listener_app,temp_listener,temp_listener_events]},
	 {applications, [kernel,stdlib]},
	 {mod, {temp_listener_app, []}},
	 {env, [
		{therms, [
			{"1081841E000000DF", "bedroom"},
			% {"10258D2A000000EA", "backyard"},
			{"10C8892A00000096", "livingroom"},
			{"101D8A2A000000F7", "machineroom"},
			{"10E8C214000000E4", "garage"},
			{"1013A51E00000035", "guestroom"},
			{"jennalynn", "jennalynn"}
			% {"2183110000C034BB", "dustinkeychain"},
			% {"21508400004025AF", "kitchen"},
			]}
	  ]}
	]}.
