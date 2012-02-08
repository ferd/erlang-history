SHELL := /bin/bash
P := $(shell erl -noshell  -eval 'io:format("~s/ebin~n",[code:lib_dir(kernel)]),init:stop().')
V := $(shell erl -noshell  -eval 'io:format("~s~n",[element(3,lists:keyfind(kernel,1,application:which_applications()))]),init:stop().')

all: compile

compile:
	erl -make

install: compile
	@echo "Backing up existing file"
	sudo cp -n "$P/group.beam" "$P/group.beam.backup-pre-shell-history"
	@echo "Installing.."
	sudo cp ebin/$V/*.beam "$P"

clean:
	rm ebin/*/*.beam
