(cl:in-package #:sicl-hir-to-cl-test)

(defun make-environment ()
  (let ((environment
          (make-instance 'sicl-alternative-extrinsic-environment:environment)))
    (sicl-alternative-extrinsic-environment::import-from-host environment)
    (sicl-hir-to-cl::fill-environment environment)
    environment))

(defun test-let (client)
  (let ((environment (make-environment))
        (form '(let ((x 10)) (+ x 20))))
    (setf (sicl-genv:fdefinition '+ environment) #'+)
    (assert (eql (eval client form environment) 30))))

(defun test-symbol-value (client)
  (let ((environment (make-environment))
        (form '(+ *x* 20)))
    (setf (sicl-genv:fdefinition '+ environment) #'+)
    (setf (sicl-genv:special-variable '*x* environment t) 10)
    (assert (eql (eval client form environment) 30))))

(defun test-block (client)
  (let ((environment (make-environment))
        (form '(let ((x 10))
                (block hello
                  (return-from hello (+ x 20))
                  50))))
    (setf (sicl-genv:fdefinition '+ environment) #'+)
    (assert (eql (eval client form environment) 30))))

(defun test (client)
  (test-let client)
  (test-symbol-value client)
  (test-block client))
