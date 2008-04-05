SHELL=/bin/sh

EFLAGS=-pa $(HOME)/lib/erlang/smtp_client-1.1/ebin/ -pa ebin

.PHONY: tgz

all: environ.boot

tgz: environ.tar.gz

ebins:
	test -d ebin || mkdir ebin
	erl $(EFLAGS) -make
	cp src/*.app ebin

environ.boot: ebins environ.rel
	erlc -W -v $(EFLAGS) environ.rel

environ.tar.gz: environ.boot
	erl $(EFLAGS) -noshell -run systools make_tar environ -run init stop

clean:
	rm -f environ.beam environ.boot environ.script environ.tar.gz
	rm -rf ebin
