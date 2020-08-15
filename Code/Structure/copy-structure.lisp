(cl:in-package #:sicl-structure)

(defun copy-structure (object)
  (check-type object structure-object)
  (let* ((class (class-of object))
         (new-object (allocate-instance class)))
    (dolist (slot (mop:class-slots class))
      ;; Use s-v-u-c here to avoid the extra slot lookup.
      (when (mop:slot-boundp-using-class class object slot)
        (setf (mop:slot-value-using-class class new-object slot)
              (mop:slot-value-using-class class object slot))))
    new-object))
