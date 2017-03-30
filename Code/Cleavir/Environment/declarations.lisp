(in-package #:cleavir-environment)

;;; This generic function takes an environment and returns a list of
;;; declaration identifers declared by the implementation or user
;;; (by (DECLAIM (DECLARATION ...)))

(defgeneric declarations (environment))

;;; An implementation should track declaim declaration, so we don't
;;; define a default empty method.

;;; But if it's called on a Cleavir environment, we should go up.
(defmethod declarations ((environment entry))
  (declarations (next environment)))
