(cl:in-package #:sicl-stream)

(defclass standard-stream (stream)
  ((%input-p :initarg :input-p :accessor input-stream-p)
   (%output-p :initarg :output-p :accessor output-stream-p)
   (%interactive-p :initform nil
                   :initarg :interactive-p
                   :accessor interactive-stream-p)
   (%element-type :initarg :element-type :accessor stream-element-type)))
