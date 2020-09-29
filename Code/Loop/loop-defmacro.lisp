(cl:in-package #:sicl-loop)

(defmacro loop (&rest forms)
  (let ((end-tag (gensym)))
    `(macrolet ((loop-finish ()
                  `(go ,',end-tag)))
       ,(expand-body forms end-tag))))
