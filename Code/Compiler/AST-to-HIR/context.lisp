(cl:in-package #:sicl-ast-to-hir)

(defclass context ()
  (;; This slot contains 0, 1, or :ALL.
   (%values-count
    :initarg :values-count
    :reader values-count)
   (%next-instruction
    :initarg :next-instruction
    :reader next-instruction)
   (%target-register
    :initarg :target-register
    :reader target-register)))

(defun make-context (values-count next-instruction target-register)
  (make-instance 'context
    :values-count values-count
    :next-instruction next-instruction
    :target-register target-register))

(defmacro with-context-components ((context-form) &body body)
  (let ((context-variable (gensym)))
    `(let* ((,context-variable ,context-form)
            (values-count (values-count ,context-variable))
            (next-instruction (next-instruction ,context-variable))
            (target-register (target-register ,context-variable)))
       (declare (ignorable values-count next-instruction target-register))
       ,@body)))
