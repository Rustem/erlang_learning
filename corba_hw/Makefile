.SUFFIXES: .erl .beam .yrl
# use vpath to tell make where to search for %.erl files
vpath %.erl gen src
# or use VPATH to tell make where to search for any prerequisite
# VPATH=src:eunit    

GEN_FILES = $(shell find gen/ -type f -name '*.erl')

ERL_OBJ = $(patsubst src/%.erl,ebin/%.beam, $(wildcard src/*erl))
ERL_OBJ += $(patsubst gen/%.erl,ebin/%.beam, $(GEN_FILES))

all: main

main: ${ERL_OBJ}

ebin/%.beam: %.erl
		erlc +debug_info -W -o ebin $<

clean:
		rm -rf ebin/*.beam