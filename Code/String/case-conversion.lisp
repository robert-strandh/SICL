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
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (loop for i from start below end
          do (setf (char string i)
                   (char-downcase (char string i)))))
  string)

(defun string-downcase (string-designator &key (start 0) end)
  (let ((string (if (characterp string-designator)
                    (string string-designator)
                    (copy-string (string string-designator)))))
    (nstring-downcase string :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-CAPITALIZE and STRING-CAPITALIZE.

(defun nstring-capitalize (string &key (start 0) (end nil))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (loop with state = nil
        for i from start below end
          for char = (char string i)
          do (if state
                 (if (alphanumericp char)
                     (setf (char string i) (char-downcase char))
                     (setf state nil))
                 (when (alphanumericp char)
                   (setf (char string i) (char-upcase char))
                   (setf state t)))))
  string)

(defun string-capitalize (string-designator &key (start 0) end)
  (let ((string (if (characterp string-designator)
                    (string string-designator)
                    (copy-string (string string-designator)))))
    (nstring-capitalize string :start start :end end)))
