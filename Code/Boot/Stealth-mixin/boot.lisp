(cl:in-package #:sicl-boot-stealth-mixin)

(defclass client (sicl-boot:client) ())

(defmethod eclector.reader:interpret-symbol :around
    ((client client) input-stream package-indicator symbol-name internp)
  (if (and (stringp package-indicator)
           (string= package-indicator "CLOSER-MOP"))
      (identity (find-symbol symbol-name (find-package '#:sicl-clos)))
      (call-next-method)))

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (ensure-asdf-system-using-client client e5 '#:stealth-mixin))))
