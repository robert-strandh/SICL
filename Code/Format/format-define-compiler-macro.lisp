(cl:in-package #:sicl-format)

(define-compiler-macro format (&whole form destination control-string &rest args)
  (if (not (stringp control-string))
      form
      (handler-case
       (let ((items (structure-items (split-control-string control-string) nil)))
         `(flet ((format-aux (stream)
                   ;; Execute the items in a new environment.
                   (let ((*destination* stream)
                         (*arguments* (vector ,@args))
                         ;; We are at the beginning of the argument vector.
                         (*next-argument-pointer* 0)
                         ;; Any unique object will do.
                         (*catch-tag* (list nil)))
                     (catch *catch-tag*
                       ,@(compile-items items)))))
            (let ((destination ,destination))
              (cond ((or (streamp destination)
                         (and (stringp destination)
                              (array-has-fill-pointer-p destination)))
                     (format-aux destination))
                    ((null destination)
                     (with-output-to-string (stream)
                                            (format-aux stream)))
                    ((eq destination t)
                     (format-aux *standard-output*))
                    (t
                     (error 'invalid-destination
                            :destination destination))))))
       (error (condition)
              (write-string "Error caught during compilation of format control string:"
                            *error-output*)
              (terpri *error-output*)
              (princ condition *error-output*)
              (terpri *error-output*)
              form))))
