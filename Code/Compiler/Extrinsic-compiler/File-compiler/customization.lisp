(cl:in-package #:sicl-extrinsic-file-compiler)

;;; Introduce immediates where appropriate.  The definition of the
;;; generic function says that we return an integer if we want to
;;; introduce an immediate value, and NIL if we do not want to
;;; introduce an immediate value.
(defmethod cleavir-generate-ast:convert-constant-to-immediate
    (constant (environment environment))
  (cond ((and (typep constant 'integer)
	      (<= #.(- (expt 2 30)) constant #.(1- (expt 2 30))))
	 (* 2 constant))
	((typep constant 'character)
	 ;; FIXME: Currently, we depend on the host having the same
	 ;; character encoding as the target.
	 (+ #b11 (* 4 (char-code constant))))
	(t
	 nil)))
