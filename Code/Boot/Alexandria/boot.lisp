(cl:in-package #:sicl-boot-alexandria)

(defclass client (sicl-boot:client) ())

(defparameter *host-function-names*
  '(;; Alexandria needs FORMAT at compile time, and since compilation
    ;; is done in the host, it needs to be the host version of FORMAT.
    ;; We could make sure Alexandria is loaded before SICL FORMAT, but
    ;; we want to be a bit more independent of loading order, so we
    ;; make it work even if SICL FORMAT has been loaded previously.
    format
    ;; It probably would not be a disaster if PARSE-DESTRUCTURING-BIND
    ;; were to be imported permanently into E5, but, we think it is
    ;; preferable to load it only for when it is needed, which is at
    ;; compile time in Alexandria.
    cleavir-code-utilities:parse-destructuring-bind
    ;; The function STRING is used at compile time by some macros like
    ;; WITH-GENSYMS and ONCE-ONLY.
    string))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (let (functions)
        (unwind-protect
             (progn
               ;; Access the current definitions in E5 of the
               ;; functions that we need to be imported from the host
               ;; during bootstrapping of Alexandria, and store them
               ;; in the lexical variable FUNCTIONS in the same order
               ;; as their names appear in *HOST-FUNCTION-NAMES*.
               ;; Note that the current definition of some function
               ;; could be NIL, which means it is undefined in E5.
               ;; What then happens is that we restore it to be
               ;; undefined after bootstrapping Alexandria.
               (setf functions
                     (loop for name in *host-function-names*
                           collect (env:fdefinition client e5 name)))
               ;; Next, import the functions named in
               ;; *HOST-FUNCTION-NAMES* into E5.
               (import-functions-from-host *host-function-names* e5)
               ;; Now, it should be safe to load Alexandria.
               (ensure-asdf-system-using-client client e5 '#:alexandria))
          ;; Restore the original definitions in E5 of the functions
          ;; named in *HOST-FUNCTION-NAMES*.
          (loop for name in *host-function-names*
                for function in functions
                do (setf (env:fdefinition client e5 name) function)))))))
