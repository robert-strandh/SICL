(cl:in-package #:sicl-loop-test)

(defmacro loop (&rest forms)
  (if (every #'consp forms)
    (let ((tag (gensym)))
      `(block nil
         (tagbody
           ,tag
           ,@forms
           (go ,tag))))
    (let ((end-tag (gensym)))
      `(macrolet ((loop-finish ()
                    `(go ,',end-tag)))
         ,(sicl-loop::expand-body forms end-tag)))))
