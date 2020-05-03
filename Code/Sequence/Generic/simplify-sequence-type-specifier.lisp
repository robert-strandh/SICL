(cl:in-package #:sicl-sequence)

(defun simplify-sequence-type-specifier (type-specifier)
  (if (not (subtypep type-specifier 'sequence))
      '(not sequence)
      (cond
        ((subtypep type-specifier 'list)
         (if (subtypep type-specifier 'null)
             'null
             'list))
        ((subtypep type-specifier 'vector)
         (replicate-for-each-relevant-vectoroid #1=#:vectoroid
           (when (subtypep type-specifier #1#)
             (return-from simplify-sequence-type-specifier (class-name #1#)))))
        (t type-specifier))))
