(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum WORD-INPUT.

(defmethod draw-datum ((datum word-input) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = lightblue, label = \"~a\"]~%"
	  (datum-id datum) (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum REGISTER-LOCATION.

(defmethod draw-datum ((datum register-location) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = red, label = \"~a\"]~%"
	  (datum-id datum) (name datum)))

