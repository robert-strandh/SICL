(cl:in-package #:cleavir-cst)

;;;; By REBUILDING a CST, we mean taking a Common Lisp expression E
;;;; and building a CST from it.  To reconstruct the source
;;;; information, an existing CST C is used.  E may contain
;;;; sub-expressions from C, and in that case the source information
;;;; in C is used for these sub-expressions.

;; (defun dictionary-from-cst (cst)
;;   (let ((result (make-hash-table :test #'eq)))
;;     (flet ((enter (expression location)
;; 	     (cond ((atom expression)
;; 		    (pushnew location (gethash expression result)))
;; 		   ((null (nth-value 1 (gethash e result)))
;; 		    (setf (gethash expression result) (list location)))
;; 		   (t
;; 		    nil))))
;;       (labels ((aux (cst)
;; 		 (let ((l (location cst))
;; 		       (e (expression cst)))
;; 		   (cond ((null (children cst))
;; 			  (pushnew l (gethash e result)))
