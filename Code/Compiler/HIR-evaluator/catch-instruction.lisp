(cl:in-package #:sicl-hir-evaluator)

;; Enumerate all catch tags for easier debugging.
(defparameter *catch-tag-counter* 0)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:catch-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :outputs 2 :successors 1)
    (let ((transfer-tag (list (incf *catch-tag-counter*)))
          (abandon-tag (list (incf *catch-tag-counter*))))
      (setf (output 0) transfer-tag)
      (setf (output 1)
            (cons (make-instance 'sicl-run-time:block/tagbody-entry
                    :frame-pointer abandon-tag)
                  dynamic-environment))
      (catch abandon-tag
        (loop for successor = (successor 0)
                then
                (catch transfer-tag
                  (let ((thunk successor))
                    (loop (setf thunk (funcall thunk (lexical-locations)))))))))))
