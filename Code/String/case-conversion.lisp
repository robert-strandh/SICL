(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-UPCASE and STRING-UPCASE.

(defun nstring-upcase (string &key (start 0) end)
  (with-checked-bounding-indices
      ((string string) (start start) (end end))
    (for-each-relevant-character
        (character string start end)
      (setf character (char-upcase character)))
    string))

(defun string-upcase (string-designator &key (start 0) end)
  (let ((string (string-designator-to-fresh-string string-designator)))
    (nstring-upcase string :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-DOWNCASE and STRING-DOWNCASE.

(defun nstring-downcase (string &key (start 0) end)
  (with-checked-bounding-indices
      ((string string) (start start) (end end))
    (for-each-relevant-character
        (character string start end)
      (setf character (char-downcase character)))
    string))

(defun string-downcase (string-designator &key (start 0) end)
  (let ((string (string-designator-to-fresh-string string-designator)))
    (nstring-downcase string :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-CAPITALIZE and STRING-CAPITALIZE.

(defun nstring-capitalize (string &key (start 0) (end nil))
  (with-checked-bounding-indices
      ((string string) (start start) (end end))
    (let ((state nil))
      (for-each-relevant-character
          (character string start end)
        (if state
            (if (alphanumericp character)
                (setf character (char-downcase character))
                (setf state nil))
            (when (alphanumericp character)
              (setf character (char-upcase character))
              (setf state t)))))
    string))

(defun string-capitalize (string-designator &key (start 0) end)
  (let ((string (string-designator-to-fresh-string string-designator)))
    (nstring-capitalize string :start start :end end)))
