(cl:in-package #:sicl-boot-sequence-functions)

(defvar *esf*)

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client)
           (esf (make-instance 'environment
                  :client client
                  :name "ESF")))
      (setf *esf* esf)
      (import-functions-from-host
       '(min
         notevery
         plusp
         logand
         subst
         fourth fifth
         nthcdr integer-length ash logior)  ; temporary
       e5)
      (setf (env:compiler-macro-function client e5 'format)
            nil)
      (let ((*environment* esf))
        (load-source-file-using-client client e5 "Array/make-array-defun.lisp")
        (ensure-asdf-system-using-client client e5 '#:fast-generic-functions)
        (ensure-asdf-system-using-client client e5 '#:sicl-utilities)
        (ensure-asdf-system-using-client client e5 '#:sicl-sequence-for-sicl-boot))
      esf)))
