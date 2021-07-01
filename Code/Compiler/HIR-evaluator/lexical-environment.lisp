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

(defstruct lref
  (depth nil :type (and unsigned-byte fixnum))
  (index nil :type (integer 0 (#.(1- array-total-size-limit)))))

(declaim (inline %lref))
(defun %lref (lexical-locations lref)
  (declare (simple-vector lexical-locations)
           (lref lref))
  (loop repeat (lref-depth lref) do
    (setf lexical-locations (svref lexical-locations 0)))
  (svref lexical-locations (lref-index lref)))

(declaim (inline (setf %lref)))
(defun (setf %lref) (value lexical-locations lref)
  (declare (simple-vector lexical-locations)
           (lref lref))
  (loop repeat (lref-depth lref) do
    (setf lexical-locations (svref lexical-locations 0)))
  (setf (svref lexical-locations (lref-index lref))
        value))

(defun lexical-environment-vector (lexical-environment parent-vector)
  (with-accessors ((counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (let ((vector (make-array counter)))
      (setf (svref vector 0) parent-vector)
      (loop for elt in contents
            for index downfrom (1- counter)
            do (setf (svref vector index) elt))
      vector)))

(defun insert-lref (entity lexical-environment)
  (with-accessors ((indices lexical-environment-indices)
                   (counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (prog1 (make-lref :index counter :depth 0)
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
          (make-lref :index index :depth 0)
          (insert-lref entity lexical-environment)))))
