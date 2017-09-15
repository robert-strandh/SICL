(cl:in-package #:cleavir-ast-graphviz)

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
;;; Drawing a CONSTANT-AST.

(defmethod stream-draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [style = filled, fillcolor = green];~%" (id ast))
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

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
;;; Drawing a TOP-LEVEL-FUNCTION-AST.

(defmethod stream-draw-ast ((ast top-level-function-ast) stream)
  (format stream "   ~a [label = \"~a ~a\"];~%"
	  (id ast)
	  (label ast)
	  (forms ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a LOAD-TIME-VALUE-AST.

(defmethod stream-draw-ast ((ast load-time-value-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast) (form ast))
  (format stream "   ~a [style = filled, fillcolor = pink];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a BIND-AST.

(defmethod stream-draw-ast ((ast bind-ast) stream)
  (format stream "   ~a [shape = box, label = \"bind\"];~%"
	  (id ast))
  (let ((symbol-id (gensym)))
    (format stream "   ~a [shape = ellipse, label = \"~a\"];~%"
	    symbol-id (symbol ast))
    (format stream "   ~a -> ~a [label = \"0\"];~%"
	    (id ast) symbol-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a THE-AST.

(defmethod stream-draw-ast ((ast the-ast) stream)
  (format stream "   ~a [label = \"the (values ~{~s ~}~@[&optional ~{~s ~}~]&rest ~s)\"];~%"
	  (id ast)
	  (cleavir-ast:required-types ast)
	  (cleavir-ast:optional-types ast)
	  (cleavir-ast:rest-type ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing an AREF-AST.

(defmethod stream-draw-ast ((ast aref-ast) stream)
  (format stream "   ~a [label = \"~:[non-simple~;simple~] aref ~s\"];~%"
	  (id ast)
	  (cleavir-ast:simple-p ast)
	  (cleavir-ast:element-type ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing an ASET-AST.

(defmethod stream-draw-ast ((ast aset-ast) stream)
  (format stream "   ~a [label = \"~:[non-simple~;simple~] aset ~s\"];~%"
	  (id ast)
	  (cleavir-ast:simple-p ast)
	  (cleavir-ast:element-type ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Labels for floating-point ASTs

(deflabel float-add-ast "float +")
(deflabel float-sub-ast "float -")
(deflabel float-mul-ast "float *")
(deflabel float-div-ast "float /")
(deflabel float-less-ast "float <")
(deflabel float-not-greater-ast "float <=")
(deflabel float-equal-ast "float =")
(deflabel float-not-less-ast "float >=")
(deflabel float-greater-ast "float >")
(deflabel float-sin-ast "float sin")
(deflabel float-cos-ast "float cos")
(deflabel float-sqrt-ast "float sqrt")
