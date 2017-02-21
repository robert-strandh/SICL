(in-package #:cleavir-kildall-graphviz)

;;;; Draw a HIR flowchart with Kildall pool annotations.
;;;; Only works for interfunction-mixin map-pool-mixin

;;; This is what specializations specialize.
;;; OBJECT is whatever specializations define for map pools.
;;; It should "draw" it by returning a string representation.
(defgeneric draw-object (specialization object))

;;; Convert a hierarchy of dictionaries into one big one.
;;; Relies on instruction-pool being gethash, etc.
(defun flatten (specialization)
  (let ((dict (make-hash-table)))
    (labels ((aux (spec)
             (maphash (lambda (k v) (setf (gethash k dict) v))
                      (dictionary spec))
             (mapc #'aux (children spec))))
      (aux specialization))
    dict))

(defun draw-flowchart-with-outputs (initial-instruction filename
                                    specialization)
  (let* ((flat (flatten specialization))
         (cleavir-ir-graphviz:*output-label-hook*
           (lambda (instruction datum number)
             (handler-case
                 (format nil "~d: ~a"
                         number
                         (draw-object specialization
                                      (find-in-pool
                                       datum
                                       (gethash instruction flat))))
               (error ()
                 ;; this happens because of ENTER instructions'
                 ;; closure env variable, if it's unused.
                 ;; those variables don't matter, so, ignorable.
                 ;(warn "Could not draw an object: ~a" e)
                 (format nil "~d" number))))))
    (cleavir-ir-graphviz:draw-flowchart
     initial-instruction filename)))

(defun draw-flowchart-with-inputs (initial-instruction filename
                                   specialization)
  (let* ((flat (flatten specialization))
         (cleavir-ir-graphviz:*input-label-hook*
           (lambda (instruction datum number)
             (format nil "~d: ~a"
                     number
                     (draw-object specialization
                                  (find-in-pool
                                   datum
                                   (gethash instruction flat)))))))
    (cleavir-ir-graphviz:draw-flowchart
     initial-instruction filename)))
