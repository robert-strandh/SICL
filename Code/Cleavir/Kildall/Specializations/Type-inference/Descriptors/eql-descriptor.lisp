(in-package #:cleavir-type-descriptors)

;;;; Constant types.
;;;; object-ltype depends on the definition of ltypes and is thus
;;;; defined in that file.

(deftype eql-descriptor ()
  '(cons (eql eql)))

(defun make-eql (object)
  `(eql ,object))

(defun eql-descriptor-object (eql-descriptor)
  (second eql-descriptor))
