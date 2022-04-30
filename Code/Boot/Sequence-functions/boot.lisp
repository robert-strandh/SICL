(cl:in-package #:sicl-boot-sequence-functions)

(defvar *esf*)

(defun invoke-with-modified-e5 (e5 esf thunk)
  (let (;; Save the existing E5 function (SETF FDEFINITION).
        (setf-fdefinition
          (env:fdefinition (env:client e5) e5 '(setf fdefinition)))
        ;; Save the existing E5 function FDEFINITION.
        (fdefinition
          (env:fdefinition (env:client e5) e5 'fdefinition))
        ;; Save the existing E5 function FBOUNDP.
        (fboundp
          (env:fdefinition (env:client e5) e5 'fboundp)))
    ;; Modify the function (SETF FDEFINITION) in E5 so that it sets
    ;; the definition in ESF instead.
    (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
          (lambda (new-function name)
            (setf (env:fdefinition (env:client esf) esf name)
                  new-function)))
    ;; Modify the function FDEFINITION in E5 so that it looks in ESF
    ;; first.
    (setf (env:fdefinition (env:client e5) e5 'fdefinition)
          (lambda (name)
            (if (null (env:fdefinition (env:client esf) esf name))
                (if (member name *sequence-function-names* :test #'equal)
                    ;; If the name is not FBOUNDP in ESF, and the name
                    ;; is that of one of the sequence functions, then
                    ;; signal an error because this should not happen, since
                    ;; client code should have called FBOUNDP first.
                    (error "Attempt to get the FDEFINITION of ~s" name)
                    ;; Otherwise, return the FDEFINITION of the name in E5.
                    (funcall fdefinition name))
                ;; If the name is FBOUNDP in ESF then return the
                ;; definition of the name in ESF.
                (env:fdefinition (env:client esf) esf name))))
    ;; Modify the function FBOUNDP in E5 so that it looks in ESF first.
    (setf (env:fdefinition (env:client e5) e5 'fboundp)
          (lambda (name)
            (or
             ;; If the name is FBOUNDP in ESF it is considered FBOUNDP
             ;; in E5.
             (not (null (env:fdefinition (env:client esf) esf name)))
             (and
              ;; If the name is not FBOUNDP in ESF and the name is
              ;; that of one of the sequence functions, then it is not
              ;; considered FBOUNDP in E5.
              (not (member name *sequence-function-names* :test #'equal))
              ;; Otherwise, it is FBOUNDP if it is FBOUNDP in E5.
              (funcall fboundp name)))))
    (unwind-protect
         (funcall thunk)
      ;; Restore the saved version of (SETF FDEFINITION) in E5.
      (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
            setf-fdefinition)
      ;; Restore the saved version of FDEFINITION in E5.
      (setf (env:fdefinition (env:client e5) e5 'fdefinition)
            fdefinition)
      ;; Restore the saved version of FBOUNDP in E5.
      (setf (env:fdefinition (env:client e5) e5 'fboundp)
            fboundp))))

;; (defmacro with-modified-e5 ((e5 esf) &body body)
;;   `(invoke-with-modified-e5
;;     ,e5 ,esf
;;     (lambda ()
;;       ,@body)))

(defmacro with-modified-e5 ((e5 esf) &body body)
  (declare (ignore e5 esf))
  `(progn ,@body))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (esf (make-instance 'environment
                  :client client
                  :name "ESF")))
      (reinitialize-instance client :environment esf :base e5)
      (setf *esf* esf)
      (import-functions-from-host
       '(min
         notevery)
       e5)
      (setf (env:compiler-macro-function client e5 'format)
            nil)
      (setf (env:find-class client esf 'standard-generic-function)
            (env:find-class client e5 'standard-generic-function))
      (with-modified-e5 (e5 esf)
        (ensure-asdf-system-using-client client e5 '#:fast-generic-functions)
        (ensure-asdf-system-using-client client e5 '#:sicl-sequence-for-sicl-boot))
      esf)))
