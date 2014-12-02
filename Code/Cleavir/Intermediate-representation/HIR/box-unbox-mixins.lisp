(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for boxing instructions.

;;; Mixin class for instructions that box unboxed data. 
(defclass box-instruction-mixin () ())

(defgeneric box-instruction-p (instruction))

(defmethod box-instruction-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod box-instruction-p ((instruction box-instruction-mixin))
  (declare (ignorable instruction))
  t)

;;; Mixin class for instructions that ubox boxed data. 
(defclass unbox-instruction-mixin () ())

(defgeneric unbox-instruction-p (instruction))

(defmethod unbox-instruction-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod unbox-instruction-p ((instruction unbox-instruction-mixin))
  (declare (ignorable instruction))
  t)

