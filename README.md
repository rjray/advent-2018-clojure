# advent-2018-clojure

This is my code for the 2018 [Advent of Code](https://adventofcode.com/2018), all solutions in [Clojure](https://clojure.org/).

All code is under the `src` directory. Each solution-file is named `dayNN.clj` and contains both puzzle solutions for that day. These are the publically-facing functions `p01` and `p02`.

The `data` directory contains the input data for each day. These files are prefixed with the day number, i.e., `NN_something.txt` where `NN` is the day number and the rest is a (semi-)descriptive name. There were no data-files for days 11 and 14.

There is one additional file under the `src` directory: `heap.clj`. This is an implementation of a heap data structure that can act as either a min-heap or max-heap. It is based on code I wrote for the Algorithms Specialization at Coursera in early 2018.

## Usage

This project is managed with [Leiningen](https://leiningen.org/). Running the following will download any dependencies and start a REPL:

```
lein repl
```

I never got around to any sort of generic/unified interface to the various days.

## License

Copyright © 2018 Randy J. Ray

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
