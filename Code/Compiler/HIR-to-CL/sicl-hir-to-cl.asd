(cl:in-package #:asdf-user)

;;;; This system takes HIR code and translates it to a very simple
;;;; form of Common Lisp code for execution in a host Common Lisp
;;;; system.
;;;;
;;;; In addition to being very simple, this code behaves in many ways
;;;; like native SICL code.  In particular, it is "untied", meaning
;;;; that it is not tied to a particular environment.  Instead, the
;;;; code has a top-level function that, when executed, ties it to a
;;;; particular environment, as described below.  When executed, The
;;;; top-level function accomplishes all the actions that must be
;;;; taken at load time.  In particular, top-level forms in the
;;;; original source code are then evaluated.
;;;;
;;;; In order for the generated Common Lisp code to be tied to a
;;;; particular environment, the top-level function is called with a
;;;; single argument, namely a function called the
;;;; FUNCTION-CELL-FINDER.  The FUNCTION-CELL-FINDER closes over some
;;;; environment E, and takes a single argument which is the name of a
;;;; function.  FUNCTION-CELL-FINDER returns the function cell in E
;;;; corresponding to the name given as an argument.

(defsystem #:sicl-hir-to-cl
  :depends-on (#:cleavir2-hir
               #:cleavir2-ast-to-hir
               #:sicl-hir-transformations
               #:sicl-extrinsic-environment
               #:sicl-ast-to-hir
               #:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "basic-block")
   (:file "dynamic-environment")
   (:file "run-time")
   (:file "sort-functions")
   (:file "find-lexical-locations")
   (:file "translate")
   (:file "translate-cons-related-instructions")
   (:file "translate-multiple-value-related-instructions")
   (:file "translate-graph-instructions")
   (:file "translate-basic-block")
   (:file "translate-catch-instruction")
   (:file "translate-array-related-instructions")
   (:file "translate-boxing-related-instructions")
   (:file "translate-fixnum-related-instructions")
   (:file "translate-argument-processing-instructions")
   (:file "hir-to-cl")
   (:file "cst-eval")))
