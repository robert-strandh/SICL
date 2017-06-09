(cl:in-package #:sicl-cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro push

(defmacro push (item place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((item-var (gensym)))
      `(let* ((,item-var ,item)
	      ,@(mapcar #'list vars vals)
	      (,(car store-vars) (cons ,item-var ,reader-form)))
	 ,writer-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro pop

(defmacro pop (place &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list vars vals)
	    (,(car store-vars) ,reader-form))
       (if (listp ,(car store-vars))
	   (prog1 (car ,(car store-vars))
	     (setq ,(car store-vars) (cdr ,(car store-vars)))
	     ,writer-form)
	   (error 'must-be-list
		  :datum ',(car store-vars)
		  :name 'pop)))))

