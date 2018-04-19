(in-package #:cleavir-value-numbering)

(defmethod cleavir-kildall-graphviz:draw-object ((s value-numbering) object)
  (princ-to-string object))

(defclass numbering ()
  ((input :initarg :input :accessor input)
   (output :initarg :output :accessor output)))

;;; Replace everything in the actual returned map, which uses sets, with numbers.
;;; NOTE: This way we lose information about where the value comes from.
;;; We don't need it, but maybe we should restore it later?
(defun dict-to-numbering (dict)
  (let* ((next-number 0)
         (map (make-hash-table :test #'equal))
         (inputs (make-hash-table :test #'eq))
         (outputs (make-hash-table :test #'eq)))
    (flet ((ensure-number (thing)
             (or (gethash thing map)
                 (setf (gethash thing map)
                       (prog1 next-number (incf next-number))))))
      (maphash (lambda (instruction alist)
                 (setf (gethash instruction inputs)
                       (loop for pair in alist
                             collecting (cons (car pair) (ensure-number (cdr pair))))
                       (gethash instruction outputs)
                       (loop for out in (cleavir-ir:outputs instruction)
                             for i from 0
                             collect (cons out
                                           (ensure-number
                                            ;; type matches specialization.lisp
                                            ;; (FIXME: cleanliness)
                                            (if (typep out '(or cleavir-ir:constant-input
                                                             cleavir-ir:immediate-input
                                                             cleavir-ir:load-time-value-input))
                                                out
                                                ;; as in transfer.lisp (FIXME cleanup)
                                                (cons out i)))))))
               dict))
    (make-instance 'numbering :input inputs :output outputs)))

(defun number-values (initial-instruction &key draw liveness)
  (let* ((traverse (make-instance 'value-numbering))
         (dict (cleavir-kildall:kildall traverse initial-instruction :liveness liveness)))
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw traverse dict))
    (dict-to-numbering dict)))
