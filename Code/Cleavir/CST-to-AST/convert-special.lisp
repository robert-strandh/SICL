(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) cst env system)
  (cst:db s (quote const) cst
    (declare (ignore quote))
    (convert-constant const env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) cst env system)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let* ((ast (cleavir-ast:make-block-ast nil :origin origin))
           (name (cst:raw name-cst))
	   (new-env (cleavir-env:add-block env name ast)))
      (setf (cleavir-ast:body-ast ast)
	    (process-progn (convert-sequence body-cst new-env system)))
      ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defmethod convert-special
    ((symbol (eql 'eval-when)) form environment system)
  (with-preserved-toplevel-ness
    (cst:db s (eval-when situations . body) form
      (declare (ignore eval-when))
      (let ((raw-situations (cst:raw situations)))
        (if (or (eq *compiler* 'cl:compile)
                (eq *compiler* 'cl:eval)
                (not *current-form-is-top-level-p*))
            (if (or (member :execute raw-situations)
                    (member 'cl:eval raw-situations))
                (process-progn
                 (convert-sequence body environment system))
                (convert (cst:cst-from-expression nil) environment system))
            (cond ((or
                    ;; CT   LT   E    Mode
                    ;; Yes  Yes  ---  ---
                    (and (or (member :compile-toplevel raw-situations)
                             (member 'cl:compile raw-situations))
                         (or (member :load-toplevel raw-situations)
                             (member 'cl:load raw-situations)))
                    ;; CT   LT   E    Mode
                    ;; No   Yes  Yes  CTT
                    (and (not (or (member :compile-toplevel raw-situations)
                                  (member 'compile raw-situations)))
                         (or (member :load-toplevel raw-situations)
                             (member 'load raw-situations))
                         (or (member :execute raw-situations)
                             (member 'eval raw-situations))
                         *compile-time-too*))
                   (let ((*compile-time-too* t))
                     (process-progn
                      (convert-sequence body environment system))))
                  ((or
                    ;; CT   LT   E    Mode
                    ;; No   Yes  Yes  NCT
                    (and (not (or (member :compile-toplevel raw-situations)
                                  (member 'compile raw-situations)))
                         (or (member :load-toplevel raw-situations)
                             (member 'load raw-situations))
                         (or (member :execute raw-situations)
                             (member 'eval raw-situations))
                         (not *compile-time-too*))
                    ;; CT   LT   E    Mode
                    ;; No   Yes  No   ---
                    (and (not (or (member :compile-toplevel raw-situations)
                                  (member 'compile raw-situations)))
                         (or (member :load-toplevel raw-situations)
                             (member 'load raw-situations))
                         (not (or (member :execute raw-situations)
                                  (member 'eval raw-situations)))))
                   (let ((*compile-time-too* nil))
                     (process-progn
                      (convert-sequence body environment system))))
                  ((or
                    ;; CT   LT   E    Mode
                    ;; Yes  No   ---  ---
                    (and (or (member :compile-toplevel raw-situations)
                             (member 'compile raw-situations))
                         (not (or (member :load-toplevel raw-situations)
                                  (member 'load raw-situations))))
                    ;; CT   LT   E    Mode
                    ;; No   No   Yes  CTT
                    (and (not (or (member :compile-toplevel raw-situations)
                                  (member 'compile raw-situations)))
                         (not (or (member :load-toplevel raw-situations)
                                  (member 'load raw-situations)))
                         (or (member :execute raw-situations)
                             (member 'eval raw-situations))
                         *compile-time-too*))
                   (cleavir-env:eval `(progn ,@(cst:raw body))
                                     environment environment)
                   (convert (cst:cst-from-expression nil)
                            environment system))
                  (t
                   (convert (cst:cst-from-expression nil)
                            environment system))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

;;; Take an environment and the CST of a single function definition,
;;; and return a new environment which is like the one passed as an
;;; argument except that it has been augmented by the local function
;;; name.
(defun augment-environment-from-fdef (environment definition-cst)
  (cst:db origin (name . rest) definition-cst
    (declare (ignore rest))
    (let* ((raw-name (cst:raw name))
           (var-ast (cleavir-ast:make-lexical-ast raw-name
                                                  :origin origin)))
      (cleavir-env:add-local-function environment raw-name var-ast))))

;;; Take an environment, a CST representing list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (environment definitions-cst)
  (loop with result = environment
	for definition-cst = definitions-cst then (cst:rest definition-cst)
        until (cst:null definition-cst)
	do (setf result
		 (augment-environment-from-fdef result definition-cst))
	finally (return result)))
