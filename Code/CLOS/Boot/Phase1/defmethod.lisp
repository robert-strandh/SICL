(cl:in-package #:sicl-clos)

;;; This macro is defined in preparation for reading the file
;;; containing the mop class hierarchy, which also contains 
;;; DEFMETHOD forms for certain accessor functions. 

(defmacro defmethod (name &rest stuff)
  `(cl:defmethod ,name ,@stuff))
