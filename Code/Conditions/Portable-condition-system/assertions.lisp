;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Case assertions - common

(defun case-failure (datum complex-type operator-name keys)
  "Signals a CASE-FAILURE error using the provided datum, the name of the case
operator, complex type specifier, and the case keys which were not matched."
  (error 'case-failure :datum datum
                       :expected-type `(,complex-type ,@keys)
                       :name operator-name
                       :possibilities keys))

(defun case-transform-t-otherwise-cases (cases)
  "Transforms T/OTHERWISE cases to prevent them from having a special effect
in CASE."
  (loop for (key . forms) in cases
        if (member key '(t otherwise))
          collect `((,key) ,@forms)
        else collect
        `(,key ,@forms)))

(defun case-accumulate-keys (cases)
  "Collects all keys from the provided cases into a single list."
  (loop for case in cases
        for key-or-keys = (first case)
        if (listp key-or-keys)
          append key-or-keys
        else
          collect key-or-keys))

;;; Case assertions - non-correctable

(defmacro ecase (keyform &rest cases)
  "Evaluates the keyform and checks if it matches any of the keys in the
provided cases. Signals an error otherwise."
  (let ((keys (case-accumulate-keys cases))
        (variable (gensym "ECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (case ,variable ,@(case-transform-t-otherwise-cases cases)
             (t (case-failure ,variable 'member 'ecase ',keys))))))

(defmacro etypecase (keyform &rest cases)
  "Evaluates the keyform and checks if it is of any of the types in the
provided cases. Signals an error otherwise."
  (let ((keys (mapcar #'first cases))
        (variable (gensym "ETYPECASE-VARIABLE")))
    `(let ((,variable ,keyform))
       (typecase ,variable ,@cases
                 (t (case-failure ,variable 'or 'etypecase ',keys))))))

;;; Correctable assertions - utilities

(defun store-value-read-evaluated-form ()
  "Queries the user for a single form to be evaluated, then reads and evaluates
that form."
  (format *query-io* "~&;; Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defmacro with-store-value-restart ((temp-var place tag) &body forms)
  "Evaluates the provided forms in an environment with a freshly established
STORE-VALUE restart. The arguments accepted by the macro are a pair of places
that should be set by the restart and the TAGBODY tag that control should be
transferred to after setting the value."
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
  "Evaluates the keyform (which must be a place) and checks if it matches any of
the keys in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
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
  "Evaluates the keyform (which must be a place) and checks if it is of any of
the types in the provided cases. Signals a correctable error otherwise, allowing
the programmer to modify the value stored in the keyform."
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
  "Reports the restart bound by ASSERT, printing the list of places supplied to
ASSERT, if any."
  (format stream "Retry assertion")
  (if names
      (format stream " with new value~P for ~{~S~^, ~}." (length names) names)
      (format stream ".")))

(defun assert-prompt (place-name value)
  "Queries the programmer whether they would like to modify the value of a given
place. If not, returns the old value; if yes, queries the programmer for an
expression to evaluate and returns its value.
\
If the old place is a symbol naming a variable, it is dynamically bound by
PROGV to its old value, so the programmer can use it in the newly evaluated
expression."
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
  "Evaluates TEST-FORM and checks if it is true; otherwise, signals a
correctable error that allows the programmer to retry the assertion. If any
places are supplied, the CONTINUE restart allows the programmer to set their
values before retrying the assertion. The optional arguments DATUM and
ARGUMENTS, if supplied, are used to report the assertion error."
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
  "Instantiates an error object suitable to signal within CHECK-TYPE, using the
provided place, value, and expected type or provided type string."
  (error
   'simple-type-error
   :datum value
   :expected-type type
   :format-control (if type-string
                       "The value of ~S is ~S, which is not ~A."
                       "The value of ~S is ~S, which is not of type ~S.")
   :format-arguments (list place value (or type-string type))))

(defmacro check-type (place type &optional type-string)
  "Evaluates PLACE and checks if its value is of the provided type. Otherwise,
signals a correctable error with an established STORE-VALUE restart that allows
the programmer to provide a new value for the place before the typecheck is
retried. The optional TYPE-STRING argument is used to construct the report for
the signaled error."
  (let ((variable (gensym "CHECK-TYPE-VARIABLE"))
        (tag (gensym "CHECK-TYPE-TAG")))
    `(let ((,variable ,place))
       (tagbody ,tag
          (unless (typep ,variable ',type)
            (with-store-value-restart (,variable ,place ,tag)
              (check-type-error ',place ,variable ',type ,type-string)))))))
