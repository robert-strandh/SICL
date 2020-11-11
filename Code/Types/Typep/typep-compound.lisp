(cl:in-package #:sicl-type)

;;; FIXME: check shape of type-specifier for each case.
(defun typep-compound (object type-specifier)
  (case (first type-specifier)
    (and
     (loop for type-spec in (rest type-specifier)
           always (typep object type-spec)))
    (or
     (loop for type-spec in (rest type-specifier)
           thereis (typep object type-spec)))
    (eql
     (eql object (second type-specifier)))
    (member
     (member object (rest type-specifier)))
    (not
     (not (typep object (second type-specifier))))
    (satisfies
     (let* ((global-environment (sicl-environment:global-environment))
            (client (sicl-environment:client global-environment)))
       (funcall (sicl-environment:fdefinition
                 client global-environment (second type-specifier))
                object)))
    (integer
     (typep-compound-integer object (rest type-specifier)))
    (t
     (error "can't handle type-specifier ~s" type-specifier))))
    
