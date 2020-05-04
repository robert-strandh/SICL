(cl:in-package #:sicl-sequence)

;;; This function is used to simplify a constant first argument of MAP or
;;; MAKE-SEQUENCE at compile time.
(defun simplify-sequence-type-specifier (type-specifier)
  (cond ((subtypep type-specifier 'nil)
         type-specifier)
        ((subtypep type-specifier 'list)
         (if (subtypep type-specifier 'null)
             'null
             'list))
        ((subtypep type-specifier 'vector)
         (replicate-for-each-relevant-vectoroid #1=#:vectoroid
           (when (subtypep type-specifier #1#)
             (return-from simplify-sequence-type-specifier (class-name #1#)))))
        (t type-specifier)))
