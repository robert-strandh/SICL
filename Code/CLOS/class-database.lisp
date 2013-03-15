(in-package #:sicl-clos)

;;; Classes use symbols as names, so we create an EQ hash table to map
;;; class names to classes.  
(defparameter *classes* (make-hash-table :test #'eq))

(defun find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((result (gethash name *classes*)))
    (if (null result)
	(if errorp
	    (error 'no-such-class-name
		   :name 'find-class
		   :datum name)
	    nil)
	result)))

;;; During bootstrapping, we need to search for metaclasses using a
;;; different method from that used to search for superclasses.
;;; Therefore, when we want a metaclass, we call this function.
;;; During bootstrapping, we redefine this function to search for the
;;; metaclass using the correct method.
(defun find-metaclass (&rest arguments)
  (apply #'find-class arguments))

;;; FIXME: check the truth of this comment.
;;; This function should check the type of its argument, but
;;; we don't have class types working yet at this point.  
(defun (setf find-class) (new-class name &optional errorp environment)
  (declare (ignore errorp environment))
  (if (null new-class)
      (remhash name *classes*)
      (setf (gethash name *classes*) new-class)))

;;; What we really want to do is this:
;; (defun (setf find-class) (new-class name &optional errorp environment)
;;   (declare (ignore errorp environment))
;;   (if (null new-class)
;;       (remhash name *classes*)
;;       (if (not (typep new-class 'class))
;; 	  (error 'must-be-class-or-nil
;; 		 :name '(setf find-class)
;; 		 :datum new-class)
;; 	  (setf (gethash name *classes*) new-class))))

