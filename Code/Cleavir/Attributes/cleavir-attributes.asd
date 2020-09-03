(in-package #:asdf-user)

;;;; Conceptual summary:

;;;; Functions have many properties not expressible in the CL type
;;;; system. For example:
;;;; 1) whether their arguments can escape
;;;; 2) whether they call their arguments
;;;; 3) whether it can be constant folded, and how to do so

;;;; This subsystem encapsulates this information in an
;;;; "attributes" object. These attributes can be stored in the
;;;; environment (so that e.g. a compiler knows that AREF has no
;;;; side effects) before making their way into ASTs and HIR
;;;; where they can be used to validate transformations.

;;;; For clients: You can use make-attributes to return attributes
;;;; from CLEAVIR-ENV:FUNCTION-INFO.

;;;; TODO: All of this stuff should be system-customizable.
;;;; And right now only boolean attributes are supported.
;;;; Per-argument attributes might be good, as would a constant-fold
;;;; attribute that includes the constant-fold function.

(defsystem :cleavir-attributes
  :components
  ((:file "packages")
   (:file "attributes" :depends-on ("packages"))))
