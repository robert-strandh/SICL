(in-package #:sicl-clos)

(defparameter *single-dispatch-table*
  (make-array 1000))

(defparameter *single-dispatch-flags*
  (make-array 1000 :initial-element nil))

;;; This is of course not as easy in practice.
(defclass class ()
  ((unique-number :initform nil :accessor unique-number)))

(defclass generic-function ()
  (;; When the dispatch offset is NIL, this means that
   ;; the dispatch information of this generic function has
   ;; not been computed yet.  
   ;; When it is not NIL, it is a number, and it is an offset
   ;; into *single-dispatch-table* and *single-dispatch-flags*
   ;; that marks the start offset of the dispatch information
   ;; for this generic function.
   (dispatch-offset :initform nil :accesssor dispatch-offset)))

