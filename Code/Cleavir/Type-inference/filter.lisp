(cl:in-package #:cleavir-type-inference)

;;;; We define a RESTRICTION to be a pair (LOCATION . TYPE), where
;;;; LOCATION is a lexical location and TYPE is a type descriptor. 
;;;; 
;;;; We define a BAG to be an unordered list of RESTRICTIONs such that
;;;; there are no duplicate LOCATIONs.  A bag that is valid at a
;;;; particular program point (before or after an instruction) and
;;;; that contains only LOCATIONs that are LIVE at that program point
;;;; is said to be MINIMAL.
;;;;
;;;; When we compute a bag as valid immediately after some instruction
;;;; I as a function of the bag that is valid immediately before I and
;;;; type of I, then it is not necessarily minimal, simply because in
;;;; order to compute a minimal bag, we also require liveness
;;;; information.  During the execution of the type inferencer, the
;;;; bag of a particular program point is always the same size, and it
;;;; contains exactly the locations that are live at that point.
;;;; After the computation we can therefore use the existing bag that
;;;; is valid immediately after I as a FILTER to remove locations that
;;;; are no longer valid.

;;; Compute a new bag from BAG by keeping only those RESTRICTIONs that
;;; have a location that is also present in FILTER.  We can take a
;;; performance shortcut, because we know that all the locations in
;;; FILTER are also present in BAG, so that if the two have the same
;;; length, then BAG is already minimal.
(defun filter (bag filter)
  (if (= (length bag) (length filter))
      bag
      (loop for restriction in bag
            when (member (first restriction) filter
                         :test #'eq :key #'first)
              collect restriction)))
