(cl:in-package #:sicl-call-site-manager)

(defmethod env:add-call-site :after
    (client environment call-site name)
  ;; For now, do nothing.
  (declare (ignore client environment call-site name))
  nil)
