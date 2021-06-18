(cl:in-package #:cleavir-type-inference)

;;; Return true if and only if two BAGs contain the same RESTRICTIONs.
;;; We do not have to be completely general because we know that the
;;; two bags have the same LOCATIONs in them.  They may differ only in
;;; the TYPE-DESCRIPTORs of some of the RESTRICTIONs.  Therefore, it
;;; suffices to check that every restriction in one of the bags is
;;; also present in the other bag.
(defun bag-equal (bag1 bag2) 
  (loop for restriction in bag1
        always (member restriction bag2 :test #'equal)))
