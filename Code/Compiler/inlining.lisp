(in-package #:sicl-compiler-phase-1)

(defgeneric children (ast))

(defmethod children ((ast constant-ast))
  '())

(defmethod children ((ast sicl-env:global-location))
  '())

(defmethod children ((ast call-ast))
  (cons (callee ast)
	(arguments ast)))

(defmethod children ((ast function-ast))
  (list (body-ast ast)))

(defmethod children ((ast block-ast))
  (list (body ast)))

(defmethod children ((ast catch-ast))
  (list (tag ast)
	(body ast)))

(defmethod children ((ast eval-when-ast))
  (list (body ast)))

(defmethod children ((ast go-ast))
  '())

(defmethod children ((ast if-ast))
  (list (test ast)
	(then ast)
	(else ast)))


(defmethod children ((ast load-time-value-ast))
  (list (form-ast ast)))


(defmethod children ((ast multiple-value-call-ast))
  (cons (function-ast ast)
	(argument-asts ast)))


(defmethod children ((ast multiple-value-prog1-ast))
  (cons (first-ast ast)
	(body-asts ast)))

(defmethod children ((ast progn-ast))
  (form-asts ast))

(defmethod children ((ast progv-ast))
  (append (symbols-ast ast)
	  (vals-ast ast)
	  (list (body-ast ast))))

(defmethod children ((ast return-from-ast))
  (list (form-ast ast)))

(defmethod children ((ast setq-ast))
  (list (location ast)
	(value-ast ast)))

(defmethod children ((ast tag-ast))
  '())

(defmethod children ((ast tagbody-ast))
  (items ast))

(defmethod children ((ast the-ast))
  (list (form-ast ast)))

(defmethod children ((ast throw-ast))
  (list (tag-ast ast)
	(form-ast ast)))

(defmethod children ((ast unwind-protect-ast))
  (cons (protected-ast ast)
	(cleanup-form-asts ast)))

(defclass ast-info ()
  ((%ast :initarg :ast :accessor ast)
   (%all-asts :initform '() :accessor all-asts)
   (%ins :initform (make-hash-table :test #'eq) :reader ins)
   (%outs :initform (make-hash-table :test #'eq) :reader outs)))

(defun find-all-asts (ast-info)
  (let ((asts '()))
    (labels ((traverse (ast)
	       (unless (member ast asts)
		 (push ast asts)
		 (mapc #'traverse (children ast)))))
      (traverse (ast ast-info)))
    (setf (all-asts ast-info) asts)))

(defun in-out-locations (ast-info)
  (let ((ins (ins ast-info))
	(outs (outs ast-info)))
    (labels ((traverse (ast)
	       (cond ((typep ast 'typed-location-ast)
		      (push ast (gethash (location ast) ins)))
		     ((typep ast 'setq-ast)
		      (push (location ast)
			    (gethash (location (location ast)) outs))
		      (traverse (value-ast ast)))
		     (t
		      (mapc #'traverse (children ast))))))
      (traverse (ast ast-info)))
    (values ins outs)))

(defun inline-call-p (ast-info funcall-ast)
  (let ((callee (callee funcall-ast))
	(ins (ins ast-info))
	(outs (outs ast-info)))
    (or (typep callee 'constant-ast)
	(and (typep (location callee) 'sicl-env:lexical-location)
	     (= 1 (length (gethash (location callee) ins)))
	     (= 1 (length (gethash (location callee) outs)))))))

(defun find-value (ast-info lexical-location)
  (let ((ast (find-if (lambda (ast)
			(and (typep ast 'setq-ast)
			     (eq (location (location ast)) lexical-location)))
		      (all-asts ast-info))))
    (value-ast ast)))

(defun function-to-inline (ast-info callee)
  (cond ((typep callee 'function-ast)
	 callee)
	((typep (location callee) 'sicl-env:lexical-location)
	 (find-value ast-info (location callee)))
	(t
	 nil)))

(defun inline-function-call (ast-info funcall-ast)
  (let ((fun (function-to-inline ast-info (callee funcall-ast))))
    (change-class funcall-ast 'progn-ast
      :form-asts
      (append (loop for param in (required (lambda-list fun))
		    for arg in (arguments funcall-ast)
		    collect (make-instance 'setq-ast
			      :location (make-instance 'typed-location-ast
					  :type t
					  :location param)
			      :value-ast arg))
	      (list (body-ast fun))))))
						     
  