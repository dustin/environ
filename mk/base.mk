.SUFFIXES: .erl .beam .app .rel .boot .script

all: $(MODULES)

clean:
	rm $(MODULES)

.erl.beam: $<
	erlc -W -v $<

.rel.boot: $<
	erlc $<

