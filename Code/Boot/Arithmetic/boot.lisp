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
        (ensure-asdf-system '#:sicl-arithmetic e5)
        nil)
      environment)))
