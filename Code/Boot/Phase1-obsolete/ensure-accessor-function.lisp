(cl:in-package #:sicl-boot-phase1)

;;; Recall that this function is called from ADD-READER/WRITER-METHOD
;;; to ensure that the generic function exists.  The functions
;;; ADD-READER/WRITER-method do not call ENSURE-GENERIC-FUNCTION
;;; directly, because during bootstrapping that is not going to be the
;;; right thing to do in phase 2.  We handle that situation by
;;; redefining ENSURE-ACCESSOR-FUNCTION in phase 2. 

(defun ensure-accessor-function (name lambda-list)
  (ensure-generic-function name :lambda-list lambda-list))
