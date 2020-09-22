(cl:in-package #:sicl-loop-test)

(defmacro loop (&rest forms)
  (let ((end-tag (gensym)))
    `(macrolet ((loop-finish ()
                  `(go ,',end-tag)))
       ,(sicl-loop::expand-body forms end-tag))))
