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

;;; Given a function name represented as a CST, return the name of a
;;; block (also as a CST) that should be associated with the function
;;; with that name.
(defun block-name-from-function-name (function-name)
  (if (cst:atom function-name)
      function-name
      (cst:second function-name)))
