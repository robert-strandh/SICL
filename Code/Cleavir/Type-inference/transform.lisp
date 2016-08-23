(in-package #:cleavir-type-inference)

(defun input (instruction types)
  (apply #'bag-join
	 (mapcar (lambda (pred)
		   (arc-bag pred instruction types))
		 (cleavir-ir:predecessors instruction))))

;;; TODO: values type passes.

;; the master. remove redundant typeqs by removing impossible
;;  branches, which can massively reduce the instruction graph.
;; The original plan was to just look through TYPES and find any
;;  arcs with NIL variables. This presents two problems. One, the
;;  dictionary only tracks live variables, so if a variable is last
;;  used by an instruction that adds type information (e.g. TYPEQ)
;;  an arc-cutter will miss it. Two, the arc cutting must proceed
;;  through predecessors to avoid leaving a one-successor
;;  instruction successorless, but it is not always obvious how to
;;  cut the path. Consider, e.g., the bad code
;;   ENTER - FIXNUM+ - CAR - F->M - RETURN
;;  Gotta insert an error somewhere.
;; This typeq-centric approach does mean that if an impossible arc
;;  can be inferred by other means than typeq, it will not be cut.
;; So far inferencing is primitive enough for this not to matter,
;;  except for THE (see TODO). We'll see what happens.
(defun prune-typeqs (initial types)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when (typep i 'cleavir-ir:typeq-instruction)
       (let ((ttype (cleavir-ir:value-type i))
	     (vtype (find-type (first (cleavir-ir:inputs i))
			       (input i types))))
	 (cond ((bottom-p (binary-meet vtype ttype))
		(cleavir-ir:bypass-instruction
		 i (second (cleavir-ir:successors i))))
	       ((bottom-p (difference vtype ttype))
		(cleavir-ir:bypass-instruction
		 i (first (cleavir-ir:successors i))))))))
   initial)
  ;; we've possibly excised huge portions of this function, so do
  ;;  some cleanup
  (cleavir-ir:set-predecessors initial)
  (cleavir-ir:reinitialize-data initial)
  initial)

;; remove redundant THE instructions
(defun prune-the (initial types)
  (let (death-row)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-instruction)
	 (when (subtypep (find-type (first (cleavir-ir:inputs i))
				    (input i types))
			 (cleavir-ir:value-type i))
	   (push i death-row))))
     initial)
    (mapc #'cleavir-ir:delete-instruction death-row))
  initial)

;; convert a THE instructions into a typeq instruction
(defun the->typeq (the-instruction)
  (change-class
   the-instruction 'cleavir-ir:typeq-instruction
   :successors (list (first (cleavir-ir:successors the-instruction))
		     ;; (error 'type-error :datum value :expected-type 'type)
		     (let ((fdef (cleavir-ir:new-temporary)))
		       (cleavir-ir:make-fdefinition-instruction
			(cleavir-ir:make-load-time-value-input ''cl:error)
			fdef
			(cleavir-ir::make-funcall-no-return-instruction
			 (list fdef
			       (cleavir-ir:make-load-time-value-input ''cl:type-error)
			       (cleavir-ir:make-load-time-value-input '':datum)
			       (first (cleavir-ir:inputs the-instruction))
			       (cleavir-ir:make-load-time-value-input '':expected-type)
			       (cleavir-ir:make-load-time-value-input
				`',(cleavir-ir:value-type the-instruction)))))))))

;; convert all of them. intended for safe code, before type inference.
(defun thes->typeqs (initial)
  (let (thes)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-instruction)
	 (push i thes)))
     initial)
    (mapc #'the->typeq thes)
    ;; we only added some branches, so we don't need to reinitialize etc.
    initial))

;; remove all THE and THE-VALUES instructions - for unsafe code
(defun delete-the (initial)
  (let (death-row)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i '(or cleavir-ir:the-instruction
			cleavir-ir:the-values-instruction))
	 (push i death-row)))
     initial)
    (mapc #'cleavir-ir:delete-instruction death-row))
  initial)
