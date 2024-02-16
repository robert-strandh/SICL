(cl:in-package #:sicl-new-boot)

;;; Read a sequence of characters consisting of a sequence of
;;; alphanumeric characters followed by a sequence of alphanumeric
;;; characters.  Return the two sequences transformed into upper-case
;;; strings as two values.

(defun read-alphanumeric (input-stream)
  (string-upcase
   (with-output-to-string (output-stream)
     (loop for character = (read-char input-stream)
           while (or (alphanumericp character)
                     (member character '(#\- #\+ #\* #\^)))
           do (write-char character output-stream)
           finally (unread-char character input-stream)))))

(defun read-symbol-components (input-stream)
  (values (read-alphanumeric input-stream)
          (progn (read-char input-stream)
                 (read-alphanumeric input-stream))))
