(cl:in-package #:sicl-sequence-test)

(defclass test ()
  ((%name :initarg :name :reader name)
   (%form :initarg :form :reader form)
   (%results :initarg :results :reader results)))

(defparameter *tests* '())

(defmacro deftest (name form &rest results)
  `(push (make-instance 'test
           :name ',name
           :form ',form
           :results ',results)
         *tests*))

(defmacro signals-error (form error-type)
  `(handler-case (eval ,form)
     (,error-type () t)
     (condition () nil)
     (:no-error (&rest values) (declare (ignore values)) nil)))

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(defun make-special-string (string &key fill adjust displace base)
  (let* ((len (length string))
         (len2 (if fill (+ len 4) len))
         (etype (if base 'base-char 'character)))
    (if displace
        (let ((s0 (make-array (+ len2 5)
                              :initial-contents
                              (concatenate 'string
                                           (make-string 2 :initial-element #\X)
                                           string
                                           (make-string (if fill 7 3)
                                                        :initial-element #\Y))
                              :element-type etype)))
          (make-array len2 :element-type etype
                      :adjustable adjust
                      :fill-pointer (if fill len nil)
                      :displaced-to s0
                      :displaced-index-offset 2))
      (make-array len2 :element-type etype
                  :initial-contents
                  (if fill (concatenate 'string string "ZZZZ") string)
                  :fill-pointer (if fill len nil)
                  :adjustable adjust))))

(defmacro do-special-strings ((var string-form &optional ret-form) &body forms)
  (let ((string (gensym))
        (fill (gensym "FILL"))
        (adjust (gensym "ADJUST"))
        (base (gensym "BASE"))
        (displace (gensym "DISPLACE")))
    `(let ((,string ,string-form))
       (dolist (,fill '(nil t) ,ret-form)
         (dolist (,adjust '(nil t))
           (dolist (,base '(nil t))
             (dolist (,displace '(nil t))
               (let ((,var (make-special-string
                            ,string
                            :fill ,fill :adjust ,adjust
                            :base ,base :displace ,displace)))
                 ,@forms))))))))

;;; Macro for testing that something is undefined but 'harmless'

(defmacro defharmless (name form)
  `(deftest ,name
     (block done
       (let ((*debugger-hook* #'(lambda (&rest args)
                                  (declare (ignore args))
                                  (return-from done :good))))
         (handler-case
          (unwind-protect (eval ',form) (return-from done :good))
          (condition () :good))))
     :good))

(defun run-test (test)
  (assert (equal (multiple-value-list (eval (form test)))
                 (results test))))

(defun sequence-test ()
  (mapc #'run-test *tests*)
  nil)
