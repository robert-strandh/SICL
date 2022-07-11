(cl:in-package #:sicl-boot-alexandria)

(defclass client (sicl-boot:client) ())

(defparameter *host-function-names*
  '(;; Alexandria needs FORMAT at compile time, and since compilation
    ;; is done in the host, it needs to be the host version of FORMAT.
    ;; We could make sure Alexandria is loaded before SICL FORMAT, but
    ;; we want to be a bit more independent of loading order, so we
    ;; make it work even if SICL FORMAT has been loaded previously.
    format
    ;; The function STRING is used at compile time by some macros like
    ;; WITH-GENSYMS and ONCE-ONLY.
    string))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (with-temporary-function-imports
          client e5 *host-function-names*
        (ensure-asdf-system-using-client client e5 '#:alexandria)))))
