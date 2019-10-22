(cl:in-package #:sicl-package)

;;; FIXME: it should be the CL-USER package.
(defparameter *package* nil)

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
