(cl:in-package #:cleavir-ir-graphviz)

;;; During the drawing process, the value of this variable is a hash
;;; table that contains data that have already been drawn.
(defparameter *datum-table* nil)

(defun datum-id (datum)
  (gethash datum *datum-table*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum CONSTANT-INPUT.

(defmethod draw-datum ((datum constant-input) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = green, label = \"~a\"]~%"
	  (datum-id datum) (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LEXICAL-LOCATION.

(defmethod draw-datum ((datum lexical-location) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = yellow, label = \"~a\"]~%"
	  (datum-id datum) (name datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum VALUES-LOCATION.

(defmethod draw-datum ((datum values-location) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = blue, label = \"V\"]~%"
	  (datum-id datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum IMMEDIATE-INPUT.

(defmethod draw-datum ((datum immediate-input) stream)
  (format stream "  ~a [shape = ellipse, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = aquamarine, label = \"~a\"]~%"
	  (datum-id datum) (value datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing datum LOAD-TIME-VALUE-INPUT.

(defmethod draw-datum ((datum load-time-value-input) stream)
  (format stream "  ~a [shape = box, style = filled];~%"
	  (datum-id datum))
  (format stream "   ~a [fillcolor = orange, label = \"~s\"]~%"
	  (datum-id datum) (form datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defun instruction-id (instruction)
  (gethash instruction *instruction-table*))

(defgeneric draw-instruction (instruction stream))

(defgeneric input-label (instruction datum number))

(defmethod input-label (instruction datum number)
  (declare (ignore instruction datum))
  (format nil "~d" number))

(defvar *input-label-hook*)

(defmethod input-label :around (instruction datum number)
  (if (boundp '*input-label-hook*)
      (funcall *input-label-hook* instruction datum number)
      (call-next-method)))

(defgeneric output-label (instruction datum number))

(defmethod output-label (instruction datum number)
  (declare (ignore instruction datum))
  (format nil "~d" number))

(defvar *output-label-hook*)

(defmethod output-label :around (instruction datum number)
  (if (boundp '*output-label-hook*)
      (funcall *output-label-hook* instruction datum number)
      (call-next-method)))

(defmethod draw-instruction :before (instruction stream)
  (format stream "  ~a [shape = box];~%"
	  (instruction-id instruction))
  ;; Draw a numbered bold arrow to each successor.
  (loop for next in (successors instruction)
	for i from 1
	do (format stream
		   "  ~a -> ~a [style = bold, label = \"~d\"];~%"
		   (instruction-id instruction)
		   (instruction-id next)
		   i))
  ;; Draw a numbered red dashed arrow from each input.
  (loop for datum in (inputs instruction)
	for i from 1
	do (draw-datum datum stream)
	   (format stream
		   "  ~a -> ~a [color = red, style = dashed, label = \"~a\"];~%"
		   (datum-id datum)
		   (instruction-id instruction)
		   (input-label instruction datum i)))
  ;; Draw a numbered blue dashed arrow to each output
  (loop for datum in (outputs instruction)
	for i from 1
	do (draw-datum datum stream)
	   (format stream
		   "  ~a -> ~a [color = blue, style = dashed, label = \"~d\"];~%"
		   (instruction-id instruction)
		   (datum-id datum)
		   (output-label instruction datum i))))

(defgeneric label (instruction))

(defmethod label (instruction)
  (class-name (class-of instruction)))

(defmethod draw-instruction (instruction stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (instruction-id instruction)
	  (label instruction)))

(defun draw-flowchart (initial-instruction filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq))
	  (*datum-table* (make-hash-table :test #'eq)))
      (format stream "digraph G {~%")
      (format stream "   start [label = \"START\"];~%")
      ;; Assign a unique ID to each instruction and each datum.
      (map-instructions-arbitrary-order
       (lambda (instruction)
	 (when (null (gethash instruction *instruction-table*))
	   (setf (gethash instruction *instruction-table*) (gensym))
	   (loop for datum in (append (inputs instruction) (outputs instruction))
		 when (null (gethash datum *datum-table*))
		   do (setf (gethash datum *datum-table*) (gensym)))))
       initial-instruction)
      ;; Draw all instructions together with inputs and outputs.
      (map-instructions-arbitrary-order
       (lambda (instruction)
	 (draw-instruction instruction stream))
       initial-instruction)
      ;; Draw a START label to indentify the initial instruction.
      (format stream "start -> ~a [style = bold];~%"
	      (instruction-id initial-instruction))
      (format stream "}~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General-purpose instructions.

(defun format-item (item)
  (cond ((symbolp item)
	 item)
	((listp item)
	 (mapcar #'format-item item))
	((typep item 'cleavir-ir:lexical-location)
	 (cleavir-ir:name item))
	(t
	 (error "unknown item in lambda list ~s" item))))

(defmethod label ((instruction enter-instruction))
  (with-output-to-string (stream)
    (format stream "enter ~a"
	    (mapcar #'format-item (cleavir-ir:lambda-list instruction)))))

(defmethod label ((instruction the-instruction))
  (format nil "the ~a" (value-type instruction)))

(defmethod label ((instruction typeq-instruction))
  (format nil "typeq ~a" (value-type instruction)))

(defmethod label ((instruction the-values-instruction))
  (format nil "the (values ~@[~{~s ~}~]~@[&optional ~{~s ~}~]~@[&rest ~s~])"
          (required-types instruction)
          (optional-types instruction)
          (rest-type instruction)))

(defmethod label ((instruction typew-instruction))
  (format nil "typew ~a" (ctype instruction)))

(defmethod label ((instruction dynamic-allocation-instruction))
  "DX")

(defmethod label ((instruction nop-instruction)) "nop")

(defmethod label ((instruction choke-instruction)) "choke")

(defmethod label ((instruction unreachable-instruction)) "unreachable")

(defmethod label ((instruction assignment-instruction)) "<-")

(defmethod label ((instruction funcall-instruction)) "funcall")

(defmethod label ((instruction funcall-no-return-instruction))
  "funcall-no-return")

(defmethod label ((instruction tailcall-instruction)) "tailcall")

(defmethod label ((instruction return-instruction)) "ret")

(defmethod label ((instruction fdefinition-instruction)) "fdefinition")

(defmethod label ((instruction constant-fdefinition-instruction))
  (format nil "c-fdef ~s" (name instruction)))

(defmethod label ((instruction constant-symbol-value-instruction))
  (format nil "c-sval ~s" (name instruction)))

(defmethod draw-instruction ((instruction enclose-instruction) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (instruction-id instruction)
          (label instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
          (gethash (code instruction) *instruction-table*)
          (instruction-id instruction)))

(defmethod label ((instruction enclose-instruction))
  (format nil "enclose~:[~; DX~]" (dynamic-extent-p instruction)))

(defmethod label ((instruction initialize-closure-instruction)) "initialize-closure")

(defmethod draw-instruction ((instruction unwind-instruction) stream)
  (format stream "   ~a [label = \"~a\"];~%"
          (instruction-id instruction)
          (label instruction))
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
          (instruction-id instruction)
          (gethash (destination instruction) *instruction-table*)))

(defmethod label ((instruction unwind-instruction)) "unwind")

(defmethod label ((instruction catch-instruction)) "catch")

(defmethod label ((instruction eq-instruction)) "eq")

(defmethod label ((instruction consp-instruction)) "consp")

(defmethod label ((instruction fixnump-instruction)) "fixnump")

(defmethod label ((instruction phi-instruction)) "phi")

(defmethod label ((instruction symbol-value-instruction)) "symbol-value")

(defmethod label ((instruction set-symbol-value-instruction)) "set-symbol-value")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fixnum instructions.

(defmethod label ((instruction fixnum-add-instruction)) "fixnum +")

(defmethod label ((instruction fixnum-sub-instruction)) "fixnum -")

(defmethod label ((instruction fixnum-less-instruction)) "fixnum <")

(defmethod label ((instruction fixnum-not-greater-instruction)) "fixnum <=")

(defmethod label ((instruction fixnum-equal-instruction)) "fixnum =")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array instructions.

(defmethod label ((instruction aref-instruction))
  (format nil "~:[non-simple~;simple~] ~s aref"
	  (simple-p instruction)
	  (element-type instruction)))

(defmethod label ((instruction aset-instruction))
  (format nil "~:[non-simple~;simple~] ~s aset"
	  (simple-p instruction)
	  (element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Box instructions.

(defmethod label ((instruction box-instruction))
  (format nil "~s box" (element-type instruction)))

(defmethod label ((instruction unbox-instruction))
  (format nil "~s unbox" (element-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-point arithmetic instructions.

(defmethod label ((instruction float-add-instruction)) "float +")
(defmethod label ((instruction float-sub-instruction)) "float -")
(defmethod label ((instruction float-mul-instruction)) "float *")
(defmethod label ((instruction float-div-instruction)) "float /")
(defmethod label ((instruction float-sin-instruction)) "float sin")
(defmethod label ((instruction float-cos-instruction)) "float cos")
(defmethod label ((instruction float-sqrt-instruction)) "float sqrt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General accessors.

(defmethod label ((instruction car-instruction)) "car")

(defmethod label ((instruction cdr-instruction)) "cdr")

(defmethod label ((instruction rplaca-instruction)) "rplaca")

(defmethod label ((instruction rplacd-instruction)) "rplacd")

(defmethod label ((instruction slot-read-instruction)) "rplacd")

(defmethod label ((instruction slot-write-instruction)) "rplacd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for multiple values.

(defmethod label ((instruction multiple-to-fixed-instruction)) "M->F")

(defmethod label ((instruction fixed-to-multiple-instruction)) "F->M")

(defmethod label ((instruction multiple-value-call-instruction)) "MVC")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions related to the static runtime environment.

(defmethod label ((instruction create-cell-instruction))
  (format nil "Create cell~:[~; DX~]"
          (dynamic-extent-p instruction)))

(defmethod label ((instruction fetch-instruction)) "Fetch")

(defmethod label ((instruction read-cell-instruction)) "Read cell")

(defmethod label ((instruction write-cell-instruction)) "Write cell")

(defmethod label ((instruction add-activation-record-instruction)) "AddAR")

(defmethod label ((instruction remove-activation-record-instruction)) "RemAR")

(defmethod label ((instruction load-from-static-environment-instruction))
  "Load")

(defmethod label ((instruction store-to-static-environment-instruction))
  "Store")
