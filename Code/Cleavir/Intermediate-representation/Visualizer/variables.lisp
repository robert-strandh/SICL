(cl:in-package #:cleavir-ir-visualizer)

(defvar *base-width*)

(defvar *base-height*)

(define-symbol-macro datum-height (+ *base-height* 4))

(define-symbol-macro datum-width (+ *base-width* 4))

(define-symbol-macro horizontal-node-separation (* 2.5 *base-width*))
