(cl:in-package #:cleavir-ir-visualizer)

(defvar *base-width*)

(defvar *base-height*)

(define-symbol-macro datum-height (round (+ *base-height* 4)))

(define-symbol-macro datum-width (round (+ *base-width* 4)))

(define-symbol-macro horizontal-node-separation (round (* 2.5 *base-width*)))

(define-symbol-macro vertical-rack-separation (round (* 5 *base-height*)))

(define-symbol-macro horizontal-column-separation (round (* 0.5 *base-width*)))
