SHELL=/bin/sh

EFLAGS=-pa $(HOME)/lib/erlang/smtp_client-1.1/ebin/ -pa ebin

.PHONY: tgz

all: environ.boot

tgz: environ.tar.gz

ebins:
	erl $(EFLAGS) -noshell -run make all -run init stop
	cp src/*.app ebin

environ.boot: ebins environ.rel
	erlc -W -v $(EFLAGS) environ.rel

environ.tar.gz: environ.boot
	erl $(EFLAGS) -noshell -run systools make_tar environ -run init stop

clean:
	rm -f environ.{beam,boot,script} environ.tar.gz
	rm -f ebin/*
