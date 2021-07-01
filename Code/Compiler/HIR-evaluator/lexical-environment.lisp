(in-package #:sicl-hir-evaluator)

;;; A lexical environment is an object we use while converting instructions
;;; to thunks.  It is used to assign each lexical location a suitable index
;;; into a vector, and to create such a vector in the first place.

(defclass lexical-environment ()
  (;; A hash table mapping from lexical locations to the corresponding
   ;; index in the vector of lexical locations.
   (%indices
    :initform (make-hash-table :test #'eq)
    :reader lexical-environment-indices
    :type hash-table)
   ;; A counter tracking the length of the list of contents.
   (%counter
    :initform 1
    :accessor lexical-environment-counter
    :type unsigned-byte)
   ;; A reversed list of the initial contents of the lexical environment.
   (%contents
    :initform '()
    :accessor lexical-environment-contents
    :type list)))

(defun make-lexical-environment ()
  (make-instance 'lexical-environment))

(declaim (inline %lref))
(defun %lref (lexical-locations lref)
  (declare (simple-vector lexical-locations)
           (fixnum lref))
  (svref lexical-locations lref))

(declaim (inline (setf %lref)))
(defun (setf %lref) (value lexical-locations lref)
  (declare (simple-vector lexical-locations)
           (fixnum lref))
  (setf (svref lexical-locations lref)
        value))

(defun lexical-environment-vector (lexical-environment)
  (with-accessors ((counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (let ((vector (make-array counter)))
      (loop for elt in contents
            for index downfrom (1- counter)
            do (setf (svref vector index) elt))
      vector)))

(defun insert-lref (entity lexical-environment)
  (with-accessors ((indices lexical-environment-indices)
                   (counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (prog1 counter
      (setf (gethash entity indices)
            counter)
      (incf counter)
      (push (if (typep entity 'cleavir-ir:constant-input)
                (cleavir-ir:value entity)
                '.unbound.)
            contents))))

(defun ensure-lref (entity lexical-environment)
  (with-accessors ((indices lexical-environment-indices))
      lexical-environment
    (multiple-value-bind (index presentp)
        (gethash entity indices)
      (if presentp
          index
          (insert-lref entity lexical-environment)))))
