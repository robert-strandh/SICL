(cl:in-package #:cleavir-cst-to-ast)

(defmethod trivial-constant-p (client object)
  ;; FIXME: This method should always return NIL.  We return T at the
  ;; moment because this effectively disables the code for hoisting
  ;; constant during file compilation.  This is only a kludge that should
  ;; be removed as soon as the hoisting code has been tested thoroughly.
  t)
