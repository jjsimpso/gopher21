Gopher21
========

Gopher21 is a gopher server written in Racket. It aims for compatibility with standard gopher clients whilst adding new features that don't break compatibility with the protocol. It is an experiment in what can be achieved with a modern gopher implementation.

## Features

* Compatible with the gophermap menu format.
* Build menus dynamically for directories without gophermaps.
* Sophisticated file type determination via a Racket implementation of the magic language(the same one used by the Unix file command).
* Full text search. Requires building an index of the corpus of text to search.

## Requirements

* Racket 6.11 or greater. Currently only tested on 6.11 but it should work with newer versions.
* Should run on Linux or Windows but only tested on Linux so far.

## Building from source

Just type 'make'. See the Makefile for sample commands to build a corpus. Run the 'main' executable to see command line arguments.


More detailed documentation coming soon.
