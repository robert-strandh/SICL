(in-package #:externals-elimination)

;;;; This operation consists of taking an abstract syntax tree, and
;;;; removing all references to "externals", i.e.:
;;;;
;;;;   * constants that can not be expressed as immediates,
;;;;
;;;;   * special variables,
;;;;
;;;;   * global functions.
;;;;
;;;; The resulting abstract syntax tree is in some ways independent of
;;;; the backend, and in other ways highly dependent.  It is
;;;; independent in that it contains only references to backend
;;;; independent objects such as lexical locations, and operators.  It
;;;; is higly dependent on the backend in that it has been decided
;;;; what is to be passed in registers to a function, and memory
;;;; layout for some objects has been decided as well.
;;;;
;;;; At the moment, we are targeting the x86 backend, and for that
;;;; backend, the following information is passed as arguments to a
;;;; function (either in registers or on the stack):
;;;;
;;;;   * The code object for the code that is executing.
;;;; 
;;;;   * The argument count as a raw machine integer.
;;;;
;;;;   * The first three arguments to the function.
;;;;
;;;;   * A pointer to the beginning of a sequence of words that stores
;;;;     arguments and return values when there are more than three of
;;;;     them.
;;;;
;;;;   * The static environment of the enclosing function. 
;;;;
;;;; The code object has a header of two words.  The first word
;;;; contains a pointer to the class object.  The second word points
;;;; to a slot vector.  The slot vector contains:
;;;;
;;;;   * The machine code as a Lisp vector of bytes. 
;;;;   
;;;;   * The "externals" vector a s Lisp vector of arbitrary Lisp
;;;;     objects.
;;;;
;;;; The fact that the resulting abstract syntax tree only contains
;;;; references to backend independent objects means that the
;;;; conversion to MIR and subsequent optimizations are also backend
;;;; independent.

