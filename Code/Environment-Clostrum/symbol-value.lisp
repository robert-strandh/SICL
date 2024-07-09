(cl:in-package #:sicl-environment)

(defun symbol-value (name &key (environment *environment*))
  (unless (symbolp name)
    (error 'type-error :datum name :expected-type 'symbol))
  (let ((cell (clostrum-sys:variable-cell *client* environment name)))
    ;; CELL might be NIL, but RT:SYMBOL-VALUE can deal with that.
    (rt:symbol-value name cell)))

(defun (setf symbol-value) (new-value name &key (environment *environment*))
  (unless (symbolp name)
    (error 'type-error :datum name :expected-type 'symbol))
  (let ((cell (clostrum-sys:variable-cell *client* environment name)))
    ;; CELL might be NIL.  We don't know whether there is a binding of
    ;; the variable on the stack.  If there is, then that binding will
    ;; be affected.  But if not, then (SETF RT:SYMBOL-VALUE) will set
    ;; the CAR of CELL to a new value, which is a problem if we don't
    ;; have a cell.  So what we do is if CELL is NIL, we supply a new
    ;; cell, and if it comes back with a changed CAR value, we
    ;; allocate a cell in the environment.
    (if (null cell)
        (let ((new-cell (cons nil nil)))
          ;; Set the CAR to something unique.
          (setf (car new-cell) new-cell)
          (setf (rt:symbol-value name new-cell)
                new-value)
          (unless (eq (car new-cell) new-cell)
            ;; We must create a global cell.
            (setf cell
                  (clostrum-sys:ensure-variable-cell
                   *client* environment name))
            ;; And we set the CAR of the created cell to the value
            ;; that the run time supplied.
            (setf (car cell) (car new-cell))))
        (setf (rt:symbol-value name cell)
              new-value)))
  new-value)
