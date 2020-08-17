(in-package #:sicl-hir-evaluator)

;;; A lexical environment is an object we use while converting instructions
;;; to thunks.  It is used to assign each lexical location a suitable index
;;; into a vector, and to create such a vector in the first place.

(defclass lexical-environment ()
  ((%indices
    :initform (make-hash-table :test #'eq)
    :reader lexical-environment-indices
    :type hash-table)
   ;; A counter tracking the length of the list of contents.
   (%counter
    :initform 0
    :accessor lexical-environment-counter
    :type unsigned-byte)
   ;; A reversed list of the initial contents of the lexical environment.
   (%contents
    :initform '()
    :accessor lexical-environment-contents
    :type list)))

(defun make-lexical-environment ()
  (make-instance 'lexical-environment))

(defun lexical-environment-vector (lexical-environment)
  (with-accessors ((counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (let ((vector (make-array counter)))
      (loop for elt in contents
            for index downfrom (1- counter)
            do (setf (svref vector index) elt))
      vector)))

(defun value-index (entity lexical-environment)
  (with-accessors ((indices lexical-environment-indices)
                   (counter lexical-environment-counter)
                   (contents lexical-environment-contents))
      lexical-environment
    (multiple-value-bind (index present-p)
        (gethash entity indices)
      (if present-p
          index
          (prog1 counter
            (setf (gethash entity indices)
                  counter)
            (incf counter)
            (push (if (typep entity 'cleavir-ir:constant-input)
                      (cleavir-ir:value entity)
                      '.unbound.)
                  contents))))))
