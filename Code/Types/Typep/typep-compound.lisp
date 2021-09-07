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
     (funcall (fdefinition (second type-specifier)) object))
    (integer
     (typep-compound-integer object (rest type-specifier)))
    (cons
     (cond ((null (rest type-specifier))
            (consp object))
           ((null (rest (rest type-specifier)))
            (and (consp object)
                 (typep (car object) (second type-specifier))))
           ((null (rest (rest (rest type-specifier))))
            (and (consp object)
                 (typep (car object) (second type-specifier))
                 (typep (cdr object) (third type-specifier))))
           (t
            (error "malformed type specifier: ~s" type-specifier))))
    (t
     (let ((expander (type-expander (first type-specifier))))
       (if (null expander)
           (error "can't handle type-specifier ~s" type-specifier)
           ;; We found an expander.  Expand TYPE-SPECIFIER and call
           ;; TYPEP recursively with the expanded type specifier.
           (typep object (funcall expander type-specifier nil)))))))
