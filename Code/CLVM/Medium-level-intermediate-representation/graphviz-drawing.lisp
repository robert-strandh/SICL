(in-package #:clvm-mir-graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a datum on a stream.

(defgeneric draw-datum (datum stream))

;;; During the drawing process, the value of this variable is a hash
;;; table that contains data that have already been drawn. 
(defparameter *datum-table* nil)

(defmethod draw-datum :around (datum stream)
  (when (null (gethash datum *datum-table*))
    (setf (gethash datum *datum-table*) (gensym))
    (format stream "  ~a [shape = ellipse, style = filled];~%"
	    (gethash datum *datum-table*))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum IMMEDIATE-INPUT.

(defmethod draw-datum ((datum immediate-input) stream)
  (format stream "   ~a [fillcolor = aquamarine, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum WORD-INPUT.

(defmethod draw-datum ((datum word-input) stream)
  (format stream "   ~a [fillcolor = lightblue, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum CONSTANT-INPUT.

(defmethod draw-datum ((datum constant-input) stream)
  (format stream "   ~a [fillcolor = green, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod draw-datum ((datum lexical-location) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod draw-datum ((datum special-location) stream)
  (format stream "   ~a [fillcolor = cyan4, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum GLOBAL-INPUT.

(defmethod draw-datum ((datum global-input) stream)
  (format stream "   ~a [fillcolor = cyan, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum EXTERNAL-INPUT.

(defmethod draw-datum ((datum external-input) stream)
  (format stream "   ~a [fillcolor = pink, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum REGISTER-LOCATION.

(defmethod draw-datum ((datum register-location) stream)
  (format stream "   ~a [fillcolor = red, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum STATIC-LOCATION.

(defmethod draw-datum ((datum static-location) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a,~a\"]~%"
	  (gethash datum *datum-table*)
	  (layer datum)
	  (index datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum DYNAMIC-LOCATION.

(defmethod draw-datum ((datum dynamic-location) stream)
  (format stream "   ~a [fillcolor = darkorchid, label = \"~a\"]~%"
	  (gethash datum *datum-table*)
	  (index datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LINKAGE-LOCATION.

(defmethod draw-datum ((datum linkage-location) stream)
  (format stream "   ~a [fillcolor = bisque, label = \"~a, ~a\"]~%"
	  (gethash datum *datum-table*)
	  (index datum)
	  (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defun unique-id (instruction)
  (gethash instruction *instruction-table*))

(defgeneric draw-instruction (instruction stream))
  
(defmethod draw-instruction :around (instruction stream)
  (when (null (unique-id instruction))
    (setf (gethash instruction *instruction-table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (unique-id instruction))
    (call-next-method)))

(defmethod draw-instruction :before ((instruction instruction) stream)
  (loop for next in (successors instruction)
	do (draw-instruction next stream))
  (loop for next in (successors instruction)
	for i from 1
	do (format stream
		   "  ~a -> ~a [style = bold, label = \"~d\"];~%"
		   (unique-id instruction)
		   (gethash next *instruction-table*)
		   i)))
  
(defmethod draw-instruction (instruction stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (unique-id instruction)
	  (class-name (class-of instruction))))

(defmethod draw-instruction :after (instruction stream)
  (loop for datum in (inputs instruction)
	for i from 1
	do (draw-datum datum stream)
	   (format stream
		   "  ~a -> ~a [color = red, style = dashed, label = \"~d\"];~%"
		   (gethash datum *datum-table*)
		   (unique-id instruction)
		   i))
  (loop for datum in (outputs instruction)
	for i from 1
	do (draw-datum datum stream)
	   (format stream
		   "  ~a -> ~a [color = blue, style = dashed, label = \"~d\"];~%"
		   (unique-id instruction)
		   (gethash datum *datum-table*)
		   i)))

(defun draw-flowchart (start filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq))
	  (*datum-table* (make-hash-table :test #'eq)))
	(format stream "digraph G {~%")
	(draw-instruction start stream)
	(format stream "}~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction ENTER-INSTRUCTION.

(defmethod draw-instruction ((instruction enter-instruction) stream)
  (format stream "   ~a [label = \"enter\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction NOP-INSTRUCTION.

(defmethod draw-instruction ((instruction nop-instruction) stream)
  (format stream "   ~a [label = \"nop\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction ASSIGNMENT-INSTRUCTION.

(defmethod draw-instruction
    ((instruction assignment-instruction) stream)
  (format stream "   ~a [label = \"<-\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction FUNCALL-INSTRUCTION.

(defmethod draw-instruction ((instruction funcall-instruction) stream)
  (format stream "   ~a [label = \"funcall\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction TAILCALL-INSTRUCTION.

(defmethod draw-instruction ((instruction tailcall-instruction) stream)
  (format stream "   ~a [label = \"tailcall\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction GET-VALUES-INSTRUCTION.

(defmethod draw-instruction ((instruction get-values-instruction) stream)
  (format stream "   ~a [label = \"get-values\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction RETURN-INSTRUCTION.

(defmethod draw-instruction ((instruction return-instruction) stream)
  (format stream "   ~a [label = \"ret\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction ENCLOSE-INSTRUCTION.

(defmethod draw-instruction ((instruction enclose-instruction) stream)
  (format stream "   ~a [label = \"enclose\"];~%"
	  (unique-id instruction))
  (draw-instruction (code instruction) stream)
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (code instruction) *instruction-table*)
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction GET-ARGCOUNT-INSTRUCTION.

(defmethod draw-instruction ((instruction get-argcount-instruction) stream)
  (format stream "   ~a [label = \"AC\", color = orange];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction GET-ARG-INSTRUCTION.

(defmethod draw-instruction ((instruction get-arg-instruction) stream)
  (format stream "   ~a [label = \"arg\", color = orange];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction LOAD-CONSTANT-INSTRUCTION.

(defmethod draw-instruction ((instruction load-constant-instruction) stream)
  (format stream "   ~a [label = \"LC ~d\"];~%"
	  (unique-id instruction)
	  (linkage-vector-index instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction LOAD-GLOBAL-INSTRUCTION.

(defmethod draw-instruction ((instruction load-global-instruction) stream)
  (format stream "   ~a [label = \"LX ~d\"];~%"
	  (unique-id instruction)
	  (linkage-vector-index instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction CATCH-INSTRUCTION.

(defmethod draw-instruction ((instruction catch-instruction) stream)
  (format stream "   ~a [label = \"catch\"];~%"
	  (unique-id instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draw instruction UNWIND-INSTRUCTION.

(defmethod draw-instruction ((instruction unwind-instruction) stream)
  (format stream "   ~a [label = \"unwind\"];~%"
	  (unique-id instruction)))
