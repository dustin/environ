# arch-tag: 2B2D9B78-A22C-11D8-9952-000A957659CC

.SUFFIXES: .erl .beam .app .rel .boot .script

all: $(MODULES)

clean:
	rm $(MODULES)

.erl.beam: $<
	erlc -W -v $<

.rel.boot: $<
	erlc $<

