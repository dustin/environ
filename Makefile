# arch-tag: AA622612-9F1C-11D8-8851-000A957659CC

SHELL=/bin/sh

DIRS=apps/temp_listener apps/lemp apps/env_alert

EFLAGS=-pa $(HOME)/lib/erlang/smtp_client-1.1/ebin/ \
	-pa apps/lemp -pa apps/temp_listener -pa apps/env_alert

.PHONY: tgz

all: environ.boot

all-subs:
	for d in $(DIRS);  do \
		(cd $$d && echo "*** Making in $$d" && $(MAKE)) \
	done
	erlc -W -v environ.erl

tgz: environ.tar.gz

environ.boot: all-subs environ.rel environ.app
	erlc -W -v $(EFLAGS) environ.rel

environ.tar.gz: environ.boot
	erl $(EFLAGS) -noshell -run systools make_tar environ -run init stop

clean:
	rm -f environ.{beam,boot,script} environ.tar.gz
	for d in $(DIRS);  do \
		(cd $$d && echo "*** Cleaning in $$d" && $(MAKE) clean) \
	done
