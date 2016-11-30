(defmacro defparameter (name initial-value &optional doc)
  (declare (ignore doc))
  `(progn
     (eval-when (:compile-toplevel)
       (ensure-defined-variable ,name))
     (eval-when (:load-toplevel :execute)
       (setf (symbol-value ,name) ,initial-value))))
