(cl:in-package #:sicl-boot)

;;; During bootstrapping, while we want to use file-compilation
;;; semantics, we don't really save the AST to a file, so we can let
;;; all constants be trivial in the sense that they don't have to be
;;; taken apart and turned into load-time code.
(defmethod cleavir-cst-to-ast:trivial-constant-p ((client client) object)
  (declare (ignore object))
  t)
