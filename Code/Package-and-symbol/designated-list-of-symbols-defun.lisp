(cl:in-package #:sicl-package)

;;; Recall that a designator for a list of symbols is either a non-nil
;;; symbol (denoting a singleton list whose element is is that non-nil
;;; symbol, or a proper list of symbols, denoting itself.  Thus the
;;; symbol NIL denotes the empty list of symbols.
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
