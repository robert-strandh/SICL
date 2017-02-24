(in-package #:cleavir-kildall)

;;;; Stuff for Kildall-ing nested functions. That is, Kildall works
;;;; otherwise, but with this we can deal with enclose and funcall
;;;; in a much more detailed way.

;;;; We start Kildall with a list where each entry is
;;;; (ENTER ENCLOSE RETURN)
;;;; where ENTER is an enter instruction;
;;;; ENCLOSE is the enclose with ENTER as its CODE, or NIL if
;;;; there is no such instruction;
;;;; RETURN is the return owned by ENTER or NIL if there isn't one.

(defstruct (entry (:type list)
                  (:constructor make-entry
                      (enter enclose return)))
  enter enclose return)

(defclass interfunction () ; abstract
  ((%entries :initarg :entries :accessor entries)))
(defclass reverse-traverse-interfunction
    (reverse-traverse interfunction)
  ())

(defun find-entry (specialization object &key (key #'identity))
  (find object (entries specialization) :key key))

;;; Compute the entries at the start.
(defmethod kildall :before ((specialization interfunction)
                            initial-instruction)
  (let ((entries (list (make-entry initial-instruction nil nil))))
    (cleavir-ir:map-instructions-by/with-owner
     (lambda (instruction owner)
       (typecase instruction
         (cleavir-ir:enclose-instruction
          (push (make-entry (cleavir-ir:code instruction)
                            instruction
                            nil)
                entries))
         (cleavir-ir:return-instruction
          ;; should always be after the entry is created, due to
          ;; the definition of m-i-b/w-o
          (setf (entry-return
                 (find owner entries :key #'entry-enter))
                instruction))))
     initial-instruction)
    (setf (entries specialization) entries)))

;;; given an ENTER instruction, and a RETURN instruction or null
;;; as described above, and their pools,
;;; return a pool representing information about
;;; the function represented by ENTER, to be put with its ENCLOSE.
(defgeneric compute-function-pool
    (specialization enter enter-pool return return-pool))

(defmethod process-transfer
    ((specialization reverse-traverse-interfunction)
     (instruction cleavir-ir:enter-instruction)
     pool)
  (let ((entry (find-entry specialization instruction
                           :key #'entry-enter)))
    (when (entry-enclose entry)
      (let ((enter (entry-enter entry))
            (ret (entry-return entry)))
        (add-work (entry-enclose entry)
                  (compute-function-pool
                   specialization
                   ;; FIXME: *dictionary* should be removed somehow
                   enter (instruction-pool enter *dictionary*)
                   ret (when ret
                         ;; if ret = nil just pass garbage
                         (instruction-pool ret *dictionary*))))))))
