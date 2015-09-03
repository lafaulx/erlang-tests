.SUFFIXES: .erl .beam
.erl.beam:
	erlc -W $<

ERL = erl

MODS = maketest

all: compile
	${ERL} -noshell -s maketest test -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump