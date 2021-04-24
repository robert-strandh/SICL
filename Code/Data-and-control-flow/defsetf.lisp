(cl:in-package #:sicl-data-and-control-flow)

;;; FIXME: handle the long form
(defmacro defsetf (access-fun update-fun-or-lambda-list &rest rest)
  (cond ((symbolp update-fun-or-lambda-list)
         (assert (or (null rest) (stringp (car rest))))
         `(define-setf-expander ,access-fun (&rest args)
            (let* ((vars (loop for arg in args collect (gensym)))
                   (store-var (gensym))
                   (writer-form `(,',update-fun-or-lambda-list
                                 ,@vars ,store-var))
                   (reader-form `(,',access-fun ,@vars)))
              (values vars
                      args
                      (list store-var)
                      writer-form
                      reader-form))))
        (t (error "can't handle the long form of defsetf yet"))))
