# foppl-compiler

This is a WIP implementation of the FOPPL compiler of the book [An Introduction to Probabilistic Programming](https://arxiv.org/abs/1809.10756).

The compiler transforms a first order programming language in Clojure syntax into an immediate graph representation that lends itself to different inference methods.

Please cite:

~~~bibtex
@misc{1809.10756,
Author = {Jan-Willem van de Meent and Brooks Paige and Hongseok Yang and Frank Wood},
Title = {An Introduction to Probabilistic Programming},
Year = {2018},
Eprint = {arXiv:1809.10756},
}
~~~

## Usage

Please take a look at the tests for now.

## TODO

- support sets
- investigate deterministic exp inlining

## License

Copyright © 2018 Christian Weilbach

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
