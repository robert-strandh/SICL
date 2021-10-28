(cl:in-package #:sicl-describe)

(defun describe (object &optional (stream-designator *standard-output*))
  (let ((stream
          (cond ((null stream-designator) *standard-output*)
                ((eq stream-designator t) *terminal-io*)
                (t stream-designator))))
    (describe-object object stream))
  (values))
