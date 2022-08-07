(cl:in-package #:sicl-boot-compile-and-tie)

(defun store-code-vector (code-vector)
  (let ((sicl-client:*client* (make-instance 'client))
        (length (length code-vector)))
    (symbol-macrolet ((client sicl-client:*client*))
      (let ((make-array (env:fdefinition client sicl-boot:*e5* 'make-array))
            (setf-aref (env:fdefinition client sicl-boot:*e5* '(setf aref))))
        (let ((result (funcall make-array length
                               :element-type '(unsigned-byte 8))))
          ;; Transfer the bytes to the target vector.
          (loop for i from 0 below length
                do (funcall setf-aref (aref code-vector i) result i))
          (sicl-boot:pointer result))))))
