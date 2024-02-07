(cl:in-package #:sicl-environment)

(defun symbol-value (name)
  (unless (symbolp name)
    (error 'type-error :datum name :expected-type 'symbol))
  (let ((cell (clostrum-sys:variable-cell *client* *environment* name)))
    ;; CELL might be NIL, but RT:SYMBOL-VALUE can deal with that.
    (rt:symbol-value name cell rt:*dynamic-environment*)))

(defun (setf symbol-value) (new-value name)
  (unless (symbolp name)
    (error 'type-error :datum name :expected-type 'symbol))
  (let ((cell (clostrum-sys:variable-cell *client* *environment* name)))
    ;; CELL might be NIL, but (SETF RT:SYMBOL-VALUE) can deal with
    ;; that.
    (setf (rt:symbol-value name cell rt:*dynamic-environment*)
          new-value)))
