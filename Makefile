.PHONY: all test clean build

all: build

build:
	dune build

run:
	dune exec exe/main.exe

test: build
	dune runtest -f

clean:
	dune clean