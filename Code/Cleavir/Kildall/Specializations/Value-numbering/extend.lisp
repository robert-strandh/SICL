(in-package #:cleavir-value-numbering)

;;; A mixin for transfer classes that use value numbers as map keys.
;;; While numbering uses liveness, actual value numbers obsolete liveness information,
;;; so it's not a subclass of liveness-mixin or anything.

(defclass numbered-before-mixin () ())

(defvar *numbering*)

(defmethod cleavir-kildall:kildall :around
    ((s numbered-before-mixin) initial-instruction &key numbering liveness)
  (let* ((*numbering*
           ;; not a default argument because it's convenient to
           ;; have :numbering nil do this too, and -p is a hassle.
           ;; Since numbering IS a live-before-mixin, we can just pass liveness on
           ;; and it will be defaulted correctly.
           (or numbering (number-values initial-instruction :liveness liveness))))
    (call-next-method)))

(defmethod cleavir-kildall:instruction-variables
    ((s numbered-before-mixin) instruction)
  (numbering.all-input-numbers *numbering* instruction))

(defun input-number (instruction input)
  (numbering.input-number *numbering* instruction input))

(defun output-number (instruction output)
  (numbering.output-number *numbering* instruction output))
