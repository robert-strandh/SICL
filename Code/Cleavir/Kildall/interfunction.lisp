(in-package #:cleavir-kildall)

;;;; Stuff for Kildall-ing nested functions.
;;;; In some situations we don't need to, like liveness. In this
;;;; case just use the normal Kildall interface.
;;;; If we do need to, we call the per-function Kildall when we
;;;; hit an ENCLOSE instruction. DICTIONARY->INFO then converts the
;;;; Kildall information into an "info", which is some
;;;; specialization thing. Infos are passed to TRANSFER-ENCLOSE.
;;;; We can either call Kildall with the pool of the enclose
;;;; instruction or not. If we don't, we only need to do one
;;;; analysis of the inner function, ever, so we store that.
;;;; Kildall-ing is expected to be very expensive so we'd like to
;;;; avoid it as much as possible.

;;; Information is stored and used through the specializations.
;;; Essentially, each is a tree where a child is an inner function
;;; (like a function-tree), plus stuff for running optimizations.
;;; See escape.lisp for an example.
(defclass interfunction-mixin ()
  ((%enter :initarg :enter :reader enter)
   (%dictionary :initarg :dictionary :accessor dictionary)
   (%children :initarg :children :accessor children
              :initform nil)))

;;; Traverses that use/have info for functions.
(defclass interfunction-info-mixin (interfunction-mixin)
  ((%info :initarg :info :accessor info)))

(defclass interfunction-once-mixin (interfunction-info-mixin) ())

;;; given an already complete (with dictionary/info) specialization
;;; call something on all the enters/dicts of it and all children.
(defun map-tree (function specialization)
  (funcall function (enter specialization)
           (dictionary specialization))
  (map nil (lambda (c) (map-tree function c))
       (children specialization)))

(defmethod transfer
    ((specialization interfunction-once-mixin)
     (instruction cleavir-ir:enclose-instruction)
     pool)
  (let* ((s (find instruction (children specialization)
                  :key #'enter)) ; if we've already analyzed, grab
         (info
           (if s
               (info s)
               ;; haven't seen this enclose before, so take a dive.
               ;; Make a new specialization for the inner function,
               ;; run Kildall recursively, edit in the info, bam.
               (let* ((enter (cleavir-ir:code instruction))
                      ;; TODO: this should be given more info
                      ;; ...and maybe a gf for make-instance here?
                      (child (make-instance
                              (class-of specialization)
                              :enter enter))
                      (dict (kildall child enter))
                      (info (dictionary->info specialization dict)))
                 ;; store the info for the next hit.
                 (setf (dictionary child) dict
                       (info child) info)
                 (push child (children specialization))
                 info))))
    (transfer-enclose specialization instruction info pool)))

(defgeneric transfer-enclose (specialization instruction info pool))

(defgeneric dictionary->info (specialization dictionary))
