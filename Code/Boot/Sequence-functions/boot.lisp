(cl:in-package #:sicl-boot-sequence-functions)

(defparameter *sequence-function-names*
  '(subseq (setf subseq)
    map
    map-into
    reduce
    count count-if count-if-not
    length
    reverse nreverse
    sort stable-sort
    find find-iif find-if-not
    position position-if position-if-not
    search
    mismatch
    replace
    substitute substitute-if substitute-if-not
    nsubstitute nsubstitute-if nsubstitute-if-not
    concatenate
    merge
    remove remove-if remove-if-not
    delete delete-if delete-if-not
    remove-duplicates delete-duplicates))

(defun invoke-with-modified-e5 (e5 esf thunk)
  (let (;; Save the existing function (SETF FDEFINITION) in E5.
        (setf-fdefinition
          (env:fdefinition (env:client e5) e5 '(setf fdefinition))))
    ;; Modify the function (SETF FDEFINITION) in E5 so that it sets
    ;; the definition in ESF instead.
    (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
          (lambda (new-function name)
            (setf (env:fdefinition (env:client esf) esf name)
                  new-function)))
    (unwind-protect
         (funcall thunk)
      ;; Restore the saved version of (SETF FDEFINITION) in E5.
      (setf (env:fdefinition (env:client e5) e5 '(setf fdefinition))
            setf-fdefinition))))

(defmacro with-modified-e5 ((e5 esf) &body body)
  `(invoke-with-modified-e5
    ,e5 ,esf
    (lambda ()
      ,@body)))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (esf (make-instance 'environment
                  :client client
                  :name "ESF")))
      (with-modified-e5 (e5 esf)
        nil)
      esf)))
