# arch-tag: AA622612-9F1C-11D8-8851-000A957659CC

MODULES=environ.beam environ_sup.beam lemp_handler.beam lemp_serv.beam \
	environ_mailer.beam temp_listener.beam environ_utilities.beam \
	buildscripts.beam

.SUFFIXES: .erl .beam .app .rel .script

.PHONY: tgz

all: environ.boot

tgz: environ.tar.gz

environ.boot: $(MODULES) environ.rel environ.app
	erl -noinput -s buildscripts makeboot -s init stop

environ.tar.gz: all
	erl -noinput -s buildscripts maketar -s init stop

clean:
	rm -f $(MODULES) environ.{boot,script} environ.tar.gz

.erl.beam: $<
	erlc -W $<
