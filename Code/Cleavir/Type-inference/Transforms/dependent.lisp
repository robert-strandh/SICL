(in-package #:cleavir-typed-transforms)

;;;; Transforms in this file actually use inference information.

;;;; TODO: values type passes.

;;; the master. remove redundant typeqs by removing impossible
;;;  branches, which can massively reduce the instruction graph.

;;; The original plan was to just look through TYPES and find any
;;;  arcs with NIL variables. This presents two problems. One, the
;;;  dictionary only tracks live variables, so if a variable is last
;;;  used by an instruction that adds type information (e.g. TYPEQ)
;;;  an arc-cutter will miss it. Two, the arc cutting must proceed
;;;  through predecessors to avoid leaving a one-successor
;;;  instruction successorless, but it is not always obvious how to
;;;  cut the path. Consider, e.g., the bad code
;;;   ENTER - FIXNUM+ - CAR - F->M - RETURN
;;;  Gotta insert an error somewhere.
;;; This typeq-centric approach does mean that if an impossible arc
;;;  can be inferred by other means than typeq, it will not be cut.
;;; So far inferencing is primitive enough for this not to matter,
;;;  except for THE (see TODO). We'll see what happens.
(defun prune-typeqs (initial types)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when (typep i 'cleavir-ir:typeq-instruction)
       (let ((ttype (cleavir-ir:value-type i))
	     (vtype (find-type (first (cleavir-ir:inputs i))
			       (instruction-input i types))))
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

;;; remove redundant THE instructions. basically just aesthetic.
(defun prune-the (initial types)
  (let (death-row)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:the-instruction)
	 (when (subtypep (find-type (first (cleavir-ir:inputs i))
				    (instruction-input i types))
			 (cleavir-ir:value-type i))
	   (push i death-row))))
     initial)
    (mapc #'cleavir-ir:delete-instruction death-row))
  initial)
