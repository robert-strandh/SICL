(cl:in-package #:sicl-boot-arithmetic)

(defvar *arithmetic-environment*)

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client)
           (environment
             (make-instance 'environment
               :client client
               :name "Arithmetic")))
      (setf *arithmetic-environment* environment)
      (import-functions-from-host
       '()
       e5)
      (setf (env:compiler-macro-function client e5 'format)
            nil)
      (let ((*environment* *arithmetic-environment*))
        (load-source-file "Arithmetic/number-defclass.lisp" e5)
        (load-source-file "Arithmetic/real-defclass.lisp" e5)
        (load-source-file "Arithmetic/rational-defclass.lisp" e5)
        (load-source-file "Arithmetic/integer-defclass.lisp" e5)
        (load-source-file "Arithmetic/fixnum-defclass.lisp" e5)
        (load-source-file "Arithmetic/bignum-defclass.lisp" e5)
        (load-source-file "Arithmetic/ratio-defclass.lisp" e5)
        (load-source-file "Arithmetic/float-defclass.lisp" e5)
        (load-source-file "Arithmetic/single-float-defclass.lisp" e5)
        (load-source-file "Arithmetic/double-float-defclass.lisp" e5)
        (load-source-file "Arithmetic/complex-defclass.lisp" e5)
        nil)
      environment)))
