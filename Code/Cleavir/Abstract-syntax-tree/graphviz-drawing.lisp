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

(defmethod label (ast)
  (class-name (class-of ast)))

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
;;; Drawing a CALL-AST.

(defmethod stream-draw-ast ((ast call-ast) stream)
  (format stream "   ~a [label = \"call\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a FUNCTION-AST.

(defmethod stream-draw-ast ((ast function-ast) stream)
  (format stream "   ~a [label = \"function\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a PROGN-AST.

(defmethod stream-draw-ast ((ast progn-ast) stream)
  (format stream "   ~a [label = \"progn\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a BLOCK-AST.

(defmethod stream-draw-ast ((ast block-ast) stream)
  (format stream "   ~a [label = \"block\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a RETURN-FROM-AST.

(defmethod stream-draw-ast ((ast return-from-ast) stream)
  (format stream "   ~a [label = \"return-from\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a SETQ-AST.

(defmethod stream-draw-ast ((ast setq-ast) stream)
  (format stream "   ~a [label = \"setq\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a TAG-AST.

(defmethod stream-draw-ast ((ast tag-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (name ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a TAGBODY-AST.

(defmethod stream-draw-ast ((ast tagbody-ast) stream)
  (format stream "   ~a [label = \"tagbody\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a GO-AST.

(defmethod stream-draw-ast ((ast go-ast) stream)
  (format stream "   ~a [label = \"go\"];~%"
	  (id ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a THE-AST.
     
(defmethod stream-draw-ast ((ast the-ast) stream)
  (format stream "   ~a [label = \"the\"];~%"
	  (id ast)))

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
;;; Drawing a IF-AST.

(defmethod stream-draw-ast ((ast if-ast) stream)
  (format stream "   ~a [label = \"if\"];~%"
	  (id ast)))
