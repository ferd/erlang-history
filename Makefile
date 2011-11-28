SHELL := /bin/bash
P := $(shell erl -noshell  -eval 'io:format("~s/ebin~n",[code:lib_dir(kernel)]),init:stop().')

all: compile

compile:
	erl -make

install: compile
	@echo "Backing up existing file"
	sudo mv -n "$P/group.beam" "$P/group.beam.backup-pre-shell-history"
	@echo "Installing.."
	sudo cp ebin/*.beam "$P"

clean:
	rm ebin/*.beam
