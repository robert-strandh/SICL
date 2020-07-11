(cl:in-package #:sicl-conditions)

(defun check-type-error (place value type type-string)
  (error
   'simple-type-error
   :datum value
   :expected-type type
   :format-control (if type-string
                       "The value of ~S is ~S, which is not ~A."
                       "The value of ~S is ~S, which is not of type ~S.")
   :format-arguments (list place value (or type-string type))))

(defmacro check-type (place type &optional type-string)
  (let ((variable (gensym "CHECK-TYPE-VARIABLE"))
        (tag (gensym "CHECK-TYPE-TAG")))
    `(let ((,variable ,place))
       (tagbody ,tag
          (unless (typep ,variable ',type)
            (with-store-value-restart (,variable ,place ,tag)
              (check-type-error ',place ,variable ',type ,type-string)))))))
