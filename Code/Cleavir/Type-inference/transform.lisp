(in-package #:cleavir-type-inference)

(defun input (instruction types)
  (apply #'bag-join
	 (mapcar (lambda (pred)
		   (gethash (cons pred instruction) types))
		 (cleavir-ir:predecessors instruction))))

;; connects the replacement, a successor of instruction, to all of
;;  instruction's predecessors (in instruction's place)
;; a generalization of cleavir-ir:delete-instruction
;; TODO: remove now-unreachable instructions.
(defun bypass (instruction replacement)
  (mapc (lambda (i)
	  (setf (cleavir-ir:predecessors replacement)
		(substitute i instruction
			    (cleavir-ir:predecessors replacement))
		(cleavir-ir:successors i)
		(substitute replacement instruction
			    (cleavir-ir:successors i))))
	(cleavir-ir:predecessors instruction)))

;; given an arc, recursively remove all instructions (which may be
;;  none) only reachable through it.
;(defun kill (from to)

;; the master
(defun prune-typeqs (initial types)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (when (typep i 'cleavir-ir:typeq-instruction)
       (let ((ttype (cleavir-ir:value-type i))
	     (vtype (find-type (first (cleavir-ir:inputs i))
			       (input i types))))
	 (cond ((bottom-p (binary-meet vtype ttype))
		(bypass i (second (cleavir-ir:successors i))))
	       ((bottom-p (difference vtype ttype))
		(bypass i (first (cleavir-ir:successors i))))))))
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

;; remove all THE instructions - for unsafe code
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
