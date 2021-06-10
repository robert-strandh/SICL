
# SICL: A new Common Lisp Implementation

This is the main source code repository for SICL. It contains the
compiler, standard library, and documentation.

## What is SICL?

SICL is a new implementation of Common Lisp. It is intentionally
divided into many implementation-independent modules that are written
in a totally or near-totally portable way, so as to allow other
implementations to incorporate these modules from SICL, rather than
having to maintain their own, perhaps implementation-specific
versions.

## Quick Start

1. Make sure you have installed the dependencies:

[the Cluster repository]:https://github.com/robert-strandh/Cluster
[the Concrete-Syntax-Tree repository]:https://github.com/s-expressionists/Concrete-Syntax-Tree
[the ctype repository]:https://github.com/s-expressionists/ctype
[the Eclector repository]:https://github.com/s-expressionists/Eclector
[the Trucler repository]:https://github.com/s-expressionists/Trucler
[the Clostrum repository]:https://github.com/s-expressionists/Clostrum
[the Incless repository]:https://github.com/s-expressionists/incless

   * A recent 64-bit version of SBCL
   * The system "cluster" from [the Cluster repository]
   * The system "concrete-syntax-tree" from [the Concrete-Syntax-Tree repository]
   * The system "eclector", from [the Eclector repository]
   * The system "trucler-reference", from [the Trucler repository]
   * The system "clostrum", from [the Clostrum repository]
   * The system "incless-intrinsic", from [the Incless repository]

The bash script `get-dependencies.sh` will do this work for you.

2. Clone the [source] with `git`:

   ```
   $ git clone https://github.com/robert-strandh/SICL
   $ cd SICL
   ```

3. Make sure the top-level directory can be found by ASDF.

4. Compile the boot system as follows:

   ```lisp
   (asdf:load-system :sicl-boot)
   ```

5. Change the package for convenience:

   ```lisp
   (in-package #:sicl-boot)
   ```

6. Create an instance of the BOOT class:

   ```lisp
   (defparameter *b* (boot))
   ```

   Bootstrapping may take a few minutes.

7. Start a REPL:

   ```lisp
   (repl *e5*)
   ```

[source]: https://github.com/robert-strandh/SICL

## Documentation

[Documentation]:https://github.com/robert-strandh/SICL/tree/master/Specification

Check the [Documentation] directory for more information.

[CONTRIBUTING.md]: https://github.com/robert-strandh/SICL/blob/master/CONTRIBUTING.md

## Getting Help and Contributing

The SICL community members are usually on various IRC channels.  There
is now a dedicated channel called #sicl, but discussion can also be
found on #commonlisp, and #clasp.  All these channels are on the
libera.chat network.

[logs]:https://irclog.tymoon.eu/libera/%23sicl

[LICENSE-BSD]:https://github.com/robert-strandh/SICL/blob/master/LICENSE-BSD

Keep up on SICL by reading the IRC [logs]

If you want to contribute SICL, please read [CONTRIBUTING.md].

## License

SICL is primarily distributed under the terms of the BSD license.

See [LICENSE-BSD] for more details.



