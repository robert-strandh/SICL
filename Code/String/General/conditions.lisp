(cl:in-package #:sicl-string)

(defgeneric report-string-condition (condition stream))

(define-condition bag-is-dotted-list (type-error)
  ()
  (:report report-string-condition))

(define-condition bag-is-circular-list (type-error)
  ()
  (:report report-string-condition))

(define-condition bag-contains-non-character (type-error)
  ()
  (:report report-string-condition))

(define-condition invalid-bounding-indices (error)
  ((%target :initarg :target :reader target)
   (%start :initarg start :reader start)
   (%end :initarg end :reader end)))
