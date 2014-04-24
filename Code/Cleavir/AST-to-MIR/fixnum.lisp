(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-+-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-+-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-+-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-+-instruction
				 (list temp1 temp2)
				 (list result)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM---AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM---AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum---instruction
				 (list temp1 temp2)
				 (list result)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-<-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-<-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-<-instruction
				 (list temp1 temp2)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-<=-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-<=-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-<=-instruction
				 (list temp1 temp2)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM->-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM->-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-<-instruction
				 (list temp2 temp1)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM->=-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM->=-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-<=-instruction
				 (list temp2 temp1)
				 successors)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-=-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum---ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-=-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-mir:make-fixnum-=-instruction
				 (list temp2 temp1)
				 successors)))))))))
