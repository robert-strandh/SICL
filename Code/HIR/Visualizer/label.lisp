(cl:in-package #:sicl-hir-visualizer)

(defgeneric label (instruction))

(defmethod label (instruction)
  (let* ((class-name (class-name (class-of instruction)))
         (name-string (string-downcase (symbol-name class-name)))
         (length (length name-string))
         (suffix-length (length "-instruction")))
    (subseq name-string 0 (- length suffix-length))))

(defun format-item (item)
  (cond ((symbolp item)
         item)
        ((listp item)
         (mapcar #'format-item item))
        ((typep item 'ir:register)
         (ir:name item))
        (t
         (error "unknown item in lambda list ~s" item))))

(defmethod label ((instruction ir:assignment-instruction)) "<-")

(defmethod label ((instruction ir:funcall-instruction)) "funcall")

(defmethod label ((instruction ir:return-instruction)) "ret")

(defmethod label ((instruction ir:unwind-instruction)) "unwind")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions related to the static runtime environment.

(defmethod label ((instruction ir:make-cell-instruction))
  (format nil "Make cell"))

(defmethod label ((instruction ir:read-cell-instruction)) "Read cell")

(defmethod label ((instruction ir:write-cell-instruction)) "Write cell")
