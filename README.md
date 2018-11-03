
# SICL: A new Common Lisp Implementation

This is the main source code repository for SICL. It contains the compiler,
standard library, and documentation.

# What is SICL?
SICL is a new implementation of Common
Lisp. It is intentionally
divided into many implementation-independent modules that are written
in a totally or near-totally portable way, so as to allow other
implementations to incorporate these modules from SICL, rather than
having to maintain their own, perhaps implementation-specific
versions. 



## Quick Start

1. Make sure you have installed the dependencies:

[the Concrete-Syntax-Tree repository]:https://github.com/robert-strandh/Concrete-Syntax-Tree
[the Eclector repository]:https://github.com/robert-strandh/Eclector

   * A recent 64-bit version of SBCL 
   * The system "concrete-syntax-tree" from [the Concrete-Syntax-Tree repository]
   * The system "eclector", also from [the Eclector repository]
   
2. Make sure your SBCL has a 10GB heap by passing --dynamic-space-size
   10000 to SBCL when it starts up.

3. Clone the [source] with `git`:

   ```
   $ git clone https://github.com/robert-strandh/SICL
   $ cd SICL 
   ```
4. Make sure the top-level directory can be found by ASDF.

5. Compile the boot system as follows:

   ````
   (asdf:load-system :sicl-boot)
   ````

6. Change the package for convenience:

   ````
   (in-package #:sicl-boot)
   ````

7. Create an instance of the BOOT class:

   ````
   (defparameter *b*
     (let ((sicl-extrinsic-environment::*cache-p* t))
        (make-instance 'boot)))
   ````

   Creating the first environment will take a few minutes.  In
   particular, it will seem that it is stuck when loading a few files,
   especially remf-defmacro.lisp.

8. Start a REPL:

   ````
   (sicl-extrinsic-environment::repl (r4 *b*) (r4 *b*))
   ````

[source]: https://github.com/robert-strandh/SICL
   

# Using SICL

1. Cleavir is an implementation-independent compilation framework for Common Lisp. To use, make sure that you that you are in the SICL directory and load the neccesary packages and file.
````
(ql:quickload '(cleavir-generate-ast  cleavir-ast-to-hir cleavir-hir-interpreter)) 
(load "Code/Cleavir/Environment/Examples/sbcl.lisp")
````
2. Now you can compile Common Lisp expressions like

````
(cleavir-hir-interpreter:interpret-hir (cleavir-sbcl-environment::compile-cleavir '(lambda ()  (+ 32 10))))
````

## Documentation
SICL releases are [here].

[Documentation]:https://github.com/robert-strandh/SICL/tree/master/Specification

Check the [Documentation] directory for more information.



[here]:https://github.com/robert-strandh/SICL/blob/master/RELEASES.md




[CONTRIBUTING.md]: https://github.com/robert-strandh/SICL/blob/master/CONTRIBUTING.md

## Getting Help and Contributing

The SICL community members are usually on various IRC channels.  There
is now a dedicated channel called [#sicl], but discussion can also be
found on [#lisp], [#clasp], and [#sicl].

[#sicl]: https://webchat.freenode.net/
[logs]:https://irclog.tymoon.eu/freenode/%23sicl
[#lisp]: https://webchat.freenode.net/
[#clasp]: https://webchat.freenode.net/
[#clim]: https://webchat.freenode.net/


[LICENSE-BSD]:https://github.com/robert-strandh/SICL/blob/master/LICENSE

Keep up on SICL by reading the IRC [logs]

If you want to contribute SICL, please read [CONTRIBUTING.md].


## License
SICL is primarily distributed under the terms of the BSD license.

See [LICENSE-BSD] for more details.



