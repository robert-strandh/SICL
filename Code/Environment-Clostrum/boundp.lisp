(cl:in-package #:sicl-environment)

(defun boundp (name &key (environment *environment*))
  (unless (symbolp name)
    (error 'type-error :datum name :expected-type 'symbol))
  (let ((cell (clostrum-sys:variable-cell *client* environment name)))
    ;; CELL might be NIL, but RT:BOUNDP can deal with that.
    (rt:boundp name cell rt:*dynamic-environment*)))
