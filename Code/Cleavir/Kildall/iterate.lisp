(in-package #:cleavir-kildall)

;;;; Sequential control. Repeatedly pop off a list.

(defclass iterate-mixin () ())

(defmethod kildall ((s iterate-mixin) initial-instruction &key)
  (loop while *work-list*
        do (transfer s (pop-work))))
