(in-package #:cleavir-ast-graphviz)

;;;; Drawing an AST. 
;;;;
;;;; We generate a Graphviz source file from the AST so that the AST
;;;; can be presented in graph form for easy inspection.

(defparameter *table* nil)

(defun id (ast)
  (symbol-name (gethash ast *table*)))

(defgeneric stream-draw-ast (ast stream))

(defun draw-ast (ast filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%   ordering = out;~%")
    (let ((*table* (make-hash-table :test #'eq)))
      (stream-draw-ast ast stream))
    (format stream "}~%")))

(defmethod stream-draw-ast :around (ast stream)
  (when (null (gethash ast *table*))
    (setf (gethash ast *table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (id ast))
    (call-next-method)
    (loop for child in (children ast)
	  for i from 1
	  do (stream-draw-ast child stream)
	     (format stream "   ~a -> ~a [label = \"~d\"];~%"
		     (id ast) (id child) i))))

(defgeneric label (ast))

;;; The default label is the lower-case version of the name of the
;;; class (as a string) with suffix -ast stripped off. 
(defmethod label (ast)
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

(defmacro deflabel (ast label)
  `(defmethod label ((ast ,ast))
     (declare (ignorable ast))
     ,label))

;;; Default method on STREAM-DRAW-AST.  This method simply calls the
;;; generic function LABEL in order to draw a label for the box.
(defmethod stream-draw-ast (ast stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast) (label ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing an IMMEDIATE-AST. 

(defmethod stream-draw-ast ((ast immediate-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = aquamarine];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a CONSTANT-AST.

(defmethod stream-draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = green];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a GLOBAL-AST.

(defmethod stream-draw-ast ((ast global-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = cyan];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a SPECIAL-AST.

(defmethod stream-draw-ast ((ast special-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = magenta];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a LEXICAL-AST.

(defmethod stream-draw-ast ((ast lexical-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = yellow];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a TAG-AST.

(defmethod stream-draw-ast ((ast tag-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a LOAD-TIME-VALUE-AST.

(defmethod stream-draw-ast ((ast load-time-value-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast) (form-ast ast))
  (format stream "   ~a [style = filled, fillcolor = pink];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Labels for floating-point ASTs

(deflabel short-float-add-ast "shf +")
(deflabel short-float-sub-ast "shf -")
(deflabel short-float-mul-ast "shf *")
(deflabel short-float-div-ast "shf /")
(deflabel short-float-less-ast "shf <")
(deflabel short-float-not-greater-ast "shf <=")
(deflabel short-float-greater-ast "shf >")
(deflabel short-float-not-less-ast "shf >=")
(deflabel short-float-equal-ast "shf =")
(deflabel short-float-sin-ast "shf sin")
(deflabel short-float-cos-ast "shf cos")
(deflabel short-float-sqrt-ast "shf sqrt")
(deflabel short-float-aref-ast "shf aref")
(deflabel short-float-aset-ast "shf aset")

