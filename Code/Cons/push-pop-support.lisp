(cl:in-package #:sicl-cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro push

(defun push-expander (client environment item place)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (sicl-environment:get-setf-expansion client environment place)
    (let ((item-var (gensym)))
      `(let* ((,item-var ,item)
	      ,@(mapcar #'list vars vals)
	      (,(car store-vars) (cons ,item-var ,reader-form)))
	 ,writer-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro pop

(defun pop-expander (client environment place)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (sicl-environment:get-setf-expansion client environment place)
    `(let* (,@(mapcar #'list vars vals)
	    (,(car store-vars) ,reader-form))
       (if (listp ,(car store-vars))
	   (prog1 (car ,(car store-vars))
	     (setq ,(car store-vars) (cdr ,(car store-vars)))
	     ,writer-form)
	   (error 'must-be-list
		  :datum ',(car store-vars)
		  :name 'pop)))))

