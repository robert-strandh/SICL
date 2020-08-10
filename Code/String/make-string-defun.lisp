(cl:in-package #:sicl-string)

(defun make-string (size &key (initial-element #\Space) (element-type 'character))
  ;; FIXME: We should probably call MAKE-INSTANCE directly.
  (make-array size :initial-element initial-element :element-type element-type))
