(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This little optimization is not in the AMOP.  When we are asked to
;;; compute the effective method of a single method, we take advantage
;;; of the class of that method to compute a more efficient effective
;;; method when we can. 

(defmethod compute-singleton-effective-method-function
    ((method method))
  (compile nil
	   `(lambda (&rest args)
	      (funcall ,(method-function method) args nil))))

;; (defmethod compute-singleton-effective-method-function
;;     ((method standard-reader-method))
;;   (let* ((direct-slot (accessor-method-slot-definition method))
	 
;; 	 (effective-slots (class-
	   
    
;;     (if (eq (slot-definition-allocation slot) :class)
;; 	(compile
;; 	 nil
;; 	 `(lambda (arg)
;; 	    (car ',(slot-definition-storage slot))))
;; 	(let ((effective-slot
	
	 

