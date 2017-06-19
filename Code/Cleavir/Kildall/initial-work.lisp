(in-package #:cleavir-kildall)

;;;; Options for where to start kildall-ing.

;;; First: Just start everywhere. Good for reverse traversal since
;;; starting from returns, some will be missed
;;; (e.g. infinite loops)

(defclass start-everywhere-mixin () ())

(defmethod kildall :around ((s start-everywhere-mixin)
                            initial-instruction &key)
  (let ((*work-list* (cleavir-ir:instructions-of-type
                      initial-instruction t)))
    (call-next-method)))

;;; Start from ENTER. Fine for forward traversal.

(defclass start-enter-mixin () ())

(defmethod kildall :around ((s start-enter-mixin)
                            initial-instruction &key)
  (let ((*work-list* (cleavir-ir:instructions-of-type
                      initial-instruction
                      'cleavir-ir:enter-instruction)))
    (call-next-method)))
