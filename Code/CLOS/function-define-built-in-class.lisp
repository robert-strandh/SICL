(cl:in-package #:sicl-clos)

(define-built-in-class function (t)
  ((%entry-point :initarg :entry-point)
   (%linkage-rack :initarg :linkage-rack)
   (%environment :initform nil :initarg :environment)))
