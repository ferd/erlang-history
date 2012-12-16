all: compile

compile:
	erl -make

install: compile
	./install.escript

clean:
	rm ebin/*/*.beam
