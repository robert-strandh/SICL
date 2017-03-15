(in-package #:cleavir-kildall-graphviz)

;;;; Draw a HIR flowchart with Kildall pool annotations.
;;;; Only works for interfunction-mixin map-pool-mixin

;;; This is what specializations specialize.
;;; OBJECT is whatever specializations define for map pools.
;;; It should "draw" it by returning a string representation.
(defgeneric draw-object (specialization object))

(defun draw-flowchart-with-outputs (initial-instruction filename
                                    specialization dictionary)
  (let* ((cleavir-ir-graphviz:*output-label-hook*
           (lambda (instruction datum number)
             (handler-case
                 (format nil "~d: ~a"
                         number
                         (draw-object specialization
                                      (find-in-pool specialization
                                       datum
                                       (instruction-pool
                                        instruction
                                        dictionary))))
               (error ()
                 ;; this happens because of ENTER instructions'
                 ;; closure env variable, if it's unused.
                 ;; those variables don't matter, so, ignorable.
                 ;(warn "Could not draw an object: ~a" e)
                 (format nil "~d" number))))))
    (cleavir-ir-graphviz:draw-flowchart
     initial-instruction filename)))

(defun draw-flowchart-with-inputs (initial-instruction filename
                                   specialization dictionary)
  (let* ((cleavir-ir-graphviz:*input-label-hook*
           (lambda (instruction datum number)
             ;; conditionalize this?
             (if (cleavir-ir:variable-p datum)
                 (format nil "~d: ~a"
                         number
                         (draw-object specialization
                                      (find-in-pool specialization
                                       datum
                                       (instruction-pool
                                        instruction
                                        dictionary))))
                 (format nil "~d" number)))))
    (cleavir-ir-graphviz:draw-flowchart
     initial-instruction filename)))
