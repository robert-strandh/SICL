(cl:in-package #:sicl-boot-sequence-functions)

(defvar *esf*)

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (esf (make-instance 'environment
                  :client client
                  :name "ESF"))
           (e5-client (env:client e5)))
      (reinitialize-instance client :environment esf :base e5)
      (setf *esf* esf)
      (import-functions-from-host
       '(min
         notevery)
       e5)
      (setf (env:compiler-macro-function client e5 'format)
            nil)
      (setf (env:find-class e5-client esf 'standard-generic-function)
            (env:find-class e5-client e5 'standard-generic-function))
      (ensure-asdf-system-using-client client e5 '#:fast-generic-functions)
      (ensure-asdf-system-using-client client e5 '#:sicl-sequence-for-sicl-boot)
      esf)))
