(cl:in-package #:sicl-format)

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (control-string)
  (loop with start = 0
        with end = (length control-string)
        while (< start end)
        collect (let ((tilde-position (position #\~ control-string :start start)))
                  (cond ((null tilde-position)
                         ;; No tilde was found.  The rest of the control string
                         ;; is just a string to be printed.
                         (prog1 (subseq control-string start end)
                           (setf start end)))
                        ((> tilde-position start)
                         ;; A tilde was found, but it is not in the
                         ;; start position.  A prefix of the control
                         ;; string is therefore a string to be
                         ;; printed.
                         (prog1 (subseq control-string start tilde-position)
                           ;; Make sure we find the tilde at the start position
                           ;; in the next iteration.
                           (setf start tilde-position)))
                        (t
                         ;; We found a tilde in the start position, so we have
                         ;; a directive.
                         (multiple-value-bind (directive-character
                                               parameters
                                               colonp
                                               at-signp
                                               end-of-directive-position)
                             (parse-format-directive control-string tilde-position)
                           (prog1 (make-instance 'directive
                                    :control-string control-string
                                    :start tilde-position
                                    :end end-of-directive-position
                                    :directive-character (char-upcase directive-character)
                                    :given-parameters parameters
                                    :colonp colonp
                                    :at-signp at-signp)
                             (setf start end-of-directive-position))))))))
