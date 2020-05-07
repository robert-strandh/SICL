(cl:in-package #:sicl-printer)


;;; See section 22.1.3.3 in the HyperSpec for the rules about
;;; how symbols are printed.

;;; FIXME: this code is incomplete.

;; (defun printer-escaping-is-enabled-p ()
;;   (or *print-readably* *print-escape))

;; (defun print-package-prefix-if-necessary (name package)
;;   (when (printer-escaping-is-enabled-p)
;;     (cond ((eq package (find-package '#:keyword))
;;            (format stream ":"))
;;           ((null package)
;;            (when *print-gensym*
;;              (format stream "#:")))
;;           ((null (nth-value 1 (find-symbol name *package*)))
;;            ;; FIXME: Maybe the package name needs to be printed with
;;            ;; escape characters and such?
;;            (format stream "~a:" (package-name package)))
;;           (t
;;            nil))))

(defun print-symbol (symbol stream)
  (let ((name (symbol-name symbol))
        (package (symbol-package symbol)))
    (format stream
            "~a:~a"
            (if (null package)
                #\#
                (package-name package))
            name)))
