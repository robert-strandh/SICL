(cl:in-package #:cleavir-compilation-policy)

;;; Public interface. Given an environment return the available
;;; policy-qualities in that environment. The returned value has is a
;;; proper list of entries of the form (NAME TYPE DEFAULT), where NAME
;;; is the name of the quality, TYPE is the type of allowable values
;;; of the policy, and DEFAULT is the value used for a declaration
;;; like (OPTIMIZE NAME).
(defgeneric policy-qualities (environment)
  (:method-combination append))

;;; Private. A list to return from policy-qualities's default
;;; method: policies cleavir itself defines. Defined by
;;; DEFINE-CLEAVIR-POLICY-QUALITY.
;;; If an implementation wants to define its own policy qualities,
;;; it should specialize POLICY-QUALITIES.
(defvar *cleavir-policy-qualities* nil)

;;; Default method. If global, use cleavir's. If not, jump up.
(defmethod policy-qualities append (environment)
  ;; FIXME
  (let ((global (cleavir-env:global-environment environment)))
    (if (eq global environment)
	*cleavir-policy-qualities*
	(policy-qualities global))))

;;; Define a cleavir policy quality, respecting redefinition.
(defun make-cleavir-policy-quality (name type default)
  (unless (typep default type)
    ;; FIXME: could be an error, but this is just a sanity check
    ;; anyway
    (warn 'bad-optimize-value :specifier `(,name ,default)
	  :expected type))
  (let ((a (assoc name *cleavir-policy-qualities*)))
    (if (null a)
	(push (list name type default) *cleavir-policy-qualities*)
	(setf (rest a) (list type default)))
    name))

;;; Defines a cleavir policy quality. This way the definition can
;;; go in the actual system (type inference defines relevant type
;;; inference policies, that sorta thing).
(defmacro define-cleavir-policy-quality (name type default)
  `(make-cleavir-policy-quality ',name ',type ',default))
