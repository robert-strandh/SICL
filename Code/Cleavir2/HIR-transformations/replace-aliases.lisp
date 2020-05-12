(cl:in-package #:cleavir-hir-transformations)

(defvar *replacement-cache*)
(defvar *assignments-to-delete*)

;;; given a replacement, return a location that all of its uses can
;;; use instead. that may be the location itself if there is no replacement.
(defun replacement (location)
  (or (gethash location *replacement-cache*)
      (setf (gethash location *replacement-cache*)
            (compute-replacement location))))

(defun ssablep (location)
  (let ((defs (cleavir-ir:defining-instructions location)))
    (= (length defs) 1)))

(defun compute-replacement (location)
  (if (and (typep location 'cleavir-ir:lexical-location) (ssablep location))
      (let ((def (first (cleavir-ir:defining-instructions location))))
        (if (typep def 'cleavir-ir:assignment-instruction)
            ;; okay, it's a temp; now, is it necessary?
            (let ((pre (first (cleavir-ir:inputs def))))
              (if (ssablep pre)
                  ;; no - get a replacement by recursion,
                  ;; and mark the assignment for deletion
                  (progn (push def *assignments-to-delete*)
                         (replacement pre))
                  ;; yes
                  location))
            location))
      location))

(defun replace-alias (location replacement)
  (loop for user in (cleavir-ir:using-instructions location)
        do (cleavir-ir:substitute-input replacement location user)))

(defun replace-aliases (initial-instruction)
  (let ((*replacement-cache* (make-hash-table :test #'eq))
        (*assignments-to-delete* nil))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (setf (cleavir-ir:dynamic-environment-location i)
             (replacement (cleavir-ir:dynamic-environment-location i)))
       (setf (cleavir-ir:inputs i)
             (mapcar #'replacement (cleavir-ir:inputs i))))
     initial-instruction)
    ;; now delete assignments with unused outputs
    (mapc #'cleavir-ir:delete-instruction *assignments-to-delete*)))
