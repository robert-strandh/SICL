;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Case assertions - common

(defun case-failure (datum complex-type operator-name keys)
  (error 'case-failure :datum datum
                       :expected-type `(,complex-type ,@keys)
                       :name operator-name
                       :possibilities keys))

(defun case-transform-t-otherwise-cases (cases)
  (loop for (key . forms) in cases
        if (member key '(t otherwise))
          collect `((,key) ,@forms)
        else collect
        `(,key ,@forms)))

(defun case-accumulate-keys (cases)
  (loop for case in cases
        for key-or-keys = (first case)
        if (listp key-or-keys)
          append key-or-keys
        else
          collect key-or-keys))

;;; Case assertions - non-correctable

(defmacro ecase (keyform &rest cases)
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "ECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (case ,variable ,@(case-transform-t-otherwise-cases cases)
             (t (case-failure ,variable 'member 'ecase ',keys))))))

(defmacro etypecase (keyform &rest cases)
  (let ((keys (mapcar #'first cases))
        (variable (gensym "ETYPECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (typecase ,variable ,@cases
                 (t (case-failure ,variable 'or 'etypecase ',keys))))))

;;; Correctable assertions - utilities

(defun store-value-read-evaluated-form ()
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((temp-var place tag) &body forms)
  (let ((report-var (gensym "STORE-VALUE-REPORT"))
        (new-value-var (gensym "NEW-VALUE"))
        (form-or-forms (if (= 1 (length forms)) (first forms) `(progn ,@forms))))
    `(flet ((,report-var (stream)
              (format stream "Supply a new value of ~S." ',place)))
       (restart-case ,form-or-forms
         (store-value (,new-value-var)
           :report ,report-var
           :interactive store-value-read-evaluated-form
           (setf ,temp-var ,new-value-var
                 ,place ,new-value-var)
           (go ,tag))))))

;;; Case assertions - correctable

(defmacro ccase (keyform &rest cases)
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "CCASE-VARIABLE"))
        (block-name (gensym "CCASE-BLOCK"))
        (tag (gensym "CCASE-TAG")))
    `(block ,block-name
       (let ((,variable ,keyform))
         (tagbody ,tag
            (return-from ,block-name
              (case ,variable ,@(case-transform-t-otherwise-cases cases)
                    (t (with-store-value-restart (,variable ,keyform ,tag)
                         (case-failure ,variable 'member 'ccase ',keys))))))))))

(defmacro ctypecase (keyform &rest cases)
  (let ((keys (mapcar #'first cases))
        (variable (gensym "CTYPECASE-VARIABLE"))
        (block-name (gensym "CTYPECASE-BLOCK"))
        (tag (gensym "CTYPECASE-TAG")))
    `(block ,block-name
       (let ((,variable ,keyform))
         (tagbody ,tag
            (return-from ,block-name
              (typecase ,keyform ,@cases
                        (t (with-store-value-restart (,variable ,keyform ,tag)
                             (case-failure ,variable 'or 'ctypecase ',keys))))))))))

;;; ASSERT

(defun assert-restart-report (names stream)
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}." (length names) names)
      (format stream ".")))

(defun assert-prompt (place-name value)
  (cond ((y-or-n-p "~&;; The old value of ~S is ~S.~%~
                    ;; Do you want to supply a new value?"
                   place-name value)
         (format *query-io* "~&;; Type a form to be evaluated:~%")
         (flet ((read-it ()
                  (format *query-io* "> ")
                  (eval (read *query-io*))))
           (cond ((symbolp place-name)
                  (format *query-io*
                          "~&;; (The old value is bound to the symbol ~S.)~%"
                          place-name)
                  (progv (list place-name) (list value) (read-it)))
                 (t (read-it)))))
        (t value)))

(defmacro assert (test-form &optional places datum &rest arguments)
  (flet ((make-place-setter (place)
           `(setf ,place (assert-prompt ',place ,place))))
    (let ((tag (gensym "ASSERT-TAG")))
      `(tagbody ,tag
          (unless ,test-form
            (restart-case ,(if datum
                               `(error ,datum ,@arguments)
                               `(error "The assertion ~S failed." ',test-form))
              (continue ()
                :report (lambda (stream)
                          (assert-restart-report ',places stream))
                ,@(mapcar #'make-place-setter places)
                (go ,tag))))))))

;;; CHECK-TYPE

(defun check-type-error (place value type type-string)
  (error
   'simple-type-error
   :datum value
   :expected-type type
   :format-control (if type-string
                       "The value of ~S is ~S, which is not ~A."
                       "The value of ~S is ~S, which is not of type ~S.")
   :format-arguments (list place value (or type-string type))))

(defmacro check-type (place type &optional type-string)
  (let ((variable (gensym "CHECK-TYPE-VARIABLE"))
        (tag (gensym "CHECK-TYPE-TAG")))
    `(let ((,variable ,place))
       (tagbody ,tag
          (unless (typep ,variable ',type)
            (with-store-value-restart (,variable ,place ,tag)
              (check-type-error ',place ,variable ',type ,type-string)))))))
