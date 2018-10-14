(cl:in-package #:sicl-clos)

;;; Update the specializer profile of a generic function according to
;;; a list of specializers of a method.
(defun update-specializer-profile (generic-function specializers class-t)
  (setf (specializer-profile generic-function)
	(loop for specializer in specializers
	      for p in (specializer-profile generic-function)
	      collect (if (eq specializer class-t) p t))))

;;; Compute a completely new specializer profile for a generic
;;; function.
(defun compute-and-set-specializer-profile (generic-function class-t)
  ;; Keep the length of the profile, but with all elements NIL.
  (setf (specializer-profile generic-function)
	(make-list (length (specializer-profile generic-function))))
  (loop for method in (generic-function-methods generic-function)
	for specializers = (method-specializers method)
	do (update-specializer-profile generic-function specializers class-t)))
