
# SICL: A new Common Lisp Implementation

This is the main source code repository for SICL.

## What is SICL?

SICL is a new implementation of Common Lisp. It is intentionally
divided into many implementation-independent modules that are written
in a totally or near-totally portable way, so as to allow other
implementations to incorporate these modules from SICL, rather than
having to maintain their own, perhaps implementation-specific
versions.

## Quick Start

1. Make sure you have installed the dependencies:

[the Clonedijk repository]:https://github.com/robert-strandh/Clonedijk
[the Concrete-Syntax-Tree repository]:https://github.com/s-expressionists/Concrete-Syntax-Tree
[the ctype repository]:https://github.com/s-expressionists/ctype
[the Eclector repository]:https://github.com/s-expressionists/Eclector
[the Khazern repository]:https://github.com/s-expressionists/Khazern
[the Trucler repository]:https://github.com/s-expressionists/Trucler
[the Clostrum repository]:https://github.com/s-expressionists/Clostrum
[the Incless repository]:https://github.com/s-expressionists/Incless
[the Invistra repository]:https://github.com/s-expressionists/Invistra
[the trivial-package-locks repository]:https://github.com/yitzchak/trivial-package-locks
[the alexandria repository]:https://gitlab.common-lisp.net/alexandria/alexandria
[the architecture.builder-protocol repository]:https://github.com/scymtym/architecture.builder-protocol.git
[the s-expression-syntax repository]:https://github.com/scymtym/s-expression-syntax.git
[the Iconoclast repository]:https://github.com/robert-strandh/Iconoclast.git
[the Common Boot repository]:https://github.com/robert-strandh/Common-boot.git
[the Common Macros repository]:https://github.com/robert-strandh/Common-macros.git
[the Parcl repository]:https://github.com/robert-strandh/Parcl.git
[the Ecclesia repository]:https://github.com/s-expressionists/Ecclesia
[the Predicament repository]:https://github.com/robert-strandh/Predicament.git
[the Regalia repository]:https://github.com/robert-strandh/Regalia.git
[the Clostrophilia repository]:https://github.com/robert-strandh/Clostrophilia.git
[the Acclimation repository]:https://github.com/s-expressionists/Acclimation

   * A recent 64-bit version of SBCL
   * The system "clonedijk", from [the Clonedijk repository]
   * The system "cluster", from [the Cluster repository]
   * The system "stealth-mixin", from [the Stealth-mixin repository]
   * The system "concrete-syntax-tree", from [the Concrete-Syntax-Tree repository]
   * The system "ctype", from [the ctype repository]
   * The system "eclector", from [the Eclector repository]
   * The system "khazern-intrinsic", from [the Khazern repository]
   * The system "trucler-reference", from [the Trucler repository]
   * The system "clostrum", from [the Clostrum repository]
   * The system "incless-intrinsic", from [the Incless repository]
   * The system "invistra/intrinsic", from [the Invistra repository]
   * The system "trivial-package-locks", from [the trivial-package-locks repository]
   * The system "alexandria", from [the alexandria repository]
   * The system "architecture.builder-protocol", from [the architecture.builder-protocol repository]
   * The system "s-expression-syntax", from [the s-expression-syntax repository]
   * The systems in [the Iconoclast repository]
   * The systems in [the Common Boot repository]
   * The systems in [the Common Macros repository]
   * The systems in [the Parcl repository]
   * The system "ecclesia", from [the Ecclesia repository]
   * The systems in [the Predicament repository]
   * The systems in [the Regalia repository]
   * The systems in [the Clostrophilia repository]
   * The system "acclimation", from [the Acclimation repository]

The bash script `get-dependencies.sh` will do this work for you.

2. Clone the [source] with `git`:

   ```
   $ git clone https://github.com/robert-strandh/SICL
   $ cd SICL
   ```

3. Make sure the top-level directory can be found by ASDF.

4. Compile the boot system as follows:

   ```lisp
   (asdf:load-system :sicl-new-boot)
   ```

5. Change the package for convenience:

   ```lisp
   (in-package #:sicl-new-boot)
   ```

6. Create an instance of the BOOT class:

   ```lisp
   (boot)
   ```

   Bootstrapping may take a few minutes.

7. Start a REPL:

   ```lisp
   (repl4)
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



