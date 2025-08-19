# Gsunit

`Gsunit` is a library for building [Gradescope](https://www.gradescope.com) autograders for [OCaml](https://ocaml.org).
It's built on top of [OUnit2](https://github.com/gildor478/ounit) and is currently (as of 2025-08-19) in it's very early stages.
I created it specifically for [CAS CS 320: Concepts of Programming Languages](https://nmmull.github.io/CS320/landing/index.html) at Boston University, but I've attempted to make the library general and extendible.
In particular, I've tried to be as true as possible to the [autograder specification](https://gradescope-autograders.readthedocs.io/en/latest/specs/) given by Gradescope.

## Usage

Currently, the only way to use `Gsunit` is to build it from source.
You can clone this repository run
```
opam install gsunit/.
```
to use it in an existing project.

This library is currently not well documented.
The file `lib/gsunit.mli` contains the simplest interface that is exposed by the library, and the directory `example` contains an example autograder.
To run the autograder you can use (from within the root directory of the project):
```
dune exec ./grade_assign0.exe
```
This will produce a JSON file that meets the autograder specification for Gradescope.
Alternatively, you can use:
```
dune exec ./grade_assign0.exe -- -ounit
```
to call OUnit's test runner directly. This will emit the usual OUnit output (which is useful for debugging).
