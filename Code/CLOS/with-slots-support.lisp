(cl:in-package #:sicl-clos)

(defun with-slots-expander (slot-entries instance-form body)
  (let ((instance-var (gensym)))
    `(let ((,instance-var ,instance-form))
       (symbol-macrolet
           ,(loop for entry in slot-entries
                  collect (if (symbolp entry)
                              `(,entry
                                (slot-value ,instance-var ,entry))
                              `(,(first entry)
                                (slot-value ,instance-var ,(second entry)))))
         ,@body))))
