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

(defclass interfunction-mixin ()
  ;; alist of enclose instructions to (dict . info).
  ;; added to as we traverse.
  ((%enclose-info :initarg :enclose-info :accessor enclose-info
                  :initform nil)))

(defclass interfunction-once-mixin (interfunction-mixin) ())

(defmethod transfer
    ((specialization interfunction-once-mixin)
     (instruction cleavir-ir:enclose-instruction)
     pool)
  (let* ((a (assoc instruction (enclose-info specialization)))
         (info
           (or (cddr a)
               ;; haven't seen this enclose before, so take a dive.
               (let* ((enter (cleavir-ir:code instruction))
                      ;; TODO: this should be given more info
                      ;; FIXME: and a fresh specialization?
                      (dict (kildall specialization enter))
                      (info (dictionary->info specialization dict)))
                 ;; store the info for the next hit.
                 (push (cons instruction (cons dict info))
                       (enclose-info specialization))
                 info))))
    (transfer-enclose specialization instruction info pool)))

(defgeneric transfer-enclose (specialization instruction info pool))

(defgeneric dictionary->info (specialization dictionary))
