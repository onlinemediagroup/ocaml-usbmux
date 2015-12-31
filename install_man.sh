#!bin/sh

./main.native --help=groff > $(opam config var man)/man1/gandalf.1
