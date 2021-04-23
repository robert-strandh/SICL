(in-package #:cleavir-kildall)

;;;; Stuff for Kildall-ing nested functions. That is, Kildall works
;;;; otherwise, but with this we can deal with enclose and funcall
;;;; in a much more detailed way.

;;;; Specifically, it's made easier to transfer from ENTER/RETURN
;;;; to ENCLOSE.

(defclass interfunction-mixin () ())

(defvar *enter-enclose*)
(defun enter-enclose (instruction)
  (gethash instruction *enter-enclose*))

(defvar *return-enclose*)
(defun return-enclose (instruction)
  (gethash instruction *return-enclose*))

(defvar *enclose-info*)
(defun enclose-info (enclose)
  (gethash enclose *enclose-info*))
(defsetf enclose-info (enclose) (new-value)
  `(setf (gethash ,enclose *enclose-info*) ,new-value))

(defun initialize-interfunction (initial-instruction)
  ;; We rely on the fact that returns are only hit after their
  ;; enclose.
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (typecase instruction
       (cleavir-ir:enclose-instruction
        (setf (gethash (cleavir-ir:code instruction)
                       *enter-enclose*)
              instruction))
       (cleavir-ir:return-instruction
        (setf (gethash instruction *return-enclose*)
              (gethash owner *enter-enclose*)))))
   initial-instruction))

(defmethod kildall :around ((specialization interfunction-mixin)
                            initial-instruction &key)
  (let ((*enter-enclose* (make-hash-table :test #'eq))
        (*return-enclose* (make-hash-table :test #'eq))
        (*enclose-info* (make-hash-table :test #'eq)))
    (initialize-interfunction initial-instruction)
    (call-next-method)))
