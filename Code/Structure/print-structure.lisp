(cl:in-package #:sicl-structure)

;;; Print-structure is separate from print-object to allow
;;; the :print-object option to override and parent print-object
;;; method and fall back to the default structure printing behaviour.
(defun print-structure (structure stream)
  (let ((class (class-of structure)))
    (write-char #\# stream)
    (write-char #\S stream)
    (write (list* (class-name class)
                  (loop for slot in (closer-mop:class-slots class)
                        ;; Leave unbound slots out as they have no value.
                        when (closer-mop:slot-boundp-using-class class structure slot)
                          collect (keywordify (closer-mop:slot-definition-name slot))
                          and collect (closer-mop:slot-value-using-class class structure slot)))
           :stream stream))
  structure)
