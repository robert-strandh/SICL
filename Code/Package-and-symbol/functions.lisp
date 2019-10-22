(cl:in-package #:sicl-package)

;;; FIXME: it should be the CL-USER package.
(defparameter *package* nil)

(defun export-one-symbol (symbol package)
  (flet ((make-external (sym)
	   (setf (external-symbols package)
		 (cons sym (external-symbols package)))))
    (cond ((member symbol (external-symbols package))
           ;; do nothing
           (return-from export-one-symbol cl:t))
          ((member symbol (internal-symbols package))
           ;; change it to be external
           (setf (internal-symbols package)
                 (remove symbol (internal-symbols package)
                         :test #'eq))
           (make-external symbol))
          (t
           (loop for used = (package-use-list package)
                   then (cdr used)
                 while (consp used)
                 do (loop for syms = (package-use-list (car used))
                            then (cdr syms)
                          do (when (eq (car syms) symbol)
                               (make-external symbol)
                               (return-from export-one-symbol t))))
           ;; come here if the symbol is not accessible
           (error "symbol ~s not accessible in package ~s"
                  (symbol-name symbol)
                  ;; FIXME: This won't work for symbols
                  ;; without a home package.
                  (package-name (symbol-package symbol)))))))

(defun proper-list-p (list)
  (integerp (ignore-errors (list-length list))))

;;; Recall that a designator for a list of symbols is either a non-nil
;;; symbol (denoting a singleton list whose element is is that non-nil
;;; symbol, or a proper list of symbols, denoting itself.  Thus the
;;; symbol NIL denotes the empty list of symbols.
;;; FIXME: check for conflicts
(defun designated-list-of-symbols (designator)
  (cond ((null designator) '())
        ((symbolp designator) (list designator))
        ((and (proper-list-p designator)
              (every #'symbolp designator))
         designator)
        (t
         (error 'not-a-valid-designator-for-list-of-symbols
                :exptected-type 'designator-for-list-of-symbols
                :datum designator))))

(defun export (symbols-designator &optional package-designator *package*)
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          do (export-one-symbol symbol package)))
  t)
