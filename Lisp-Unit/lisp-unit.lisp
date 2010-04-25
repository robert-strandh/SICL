;;;-*- Mode: Lisp; Package: LISP-UNIT -*-

#|
Copyright (c) 2004-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#


;;; A test suite package, modelled after JUnit.
;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;
;;; 04/07/06 added ~<...~> to remaining error output forms [CKR]
;;; 04/06/06 added ~<...~> to compact error output better [CKR]
;;; 04/06/06 fixed RUN-TESTS to get tests dynamically (bug reported
;;;          by Daniel Edward Burke) [CKR]
;;; 02/08/06 added newlines to error output [CKR]
;;; 12/30/05 renamed ASSERT-PREDICATE to ASSERT-EQUALITY [CKR]
;;; 12/29/05 added ASSERT-EQ, ASSERT-EQL, ASSERT-EQUALP [CKR]
;;; 12/22/05 recoded use-debugger to use handler-bind, added option to prompt for debugger, 
;;; 11/07/05 added *use-debugger* and assert-predicate [DFB]
;;; 09/18/05 replaced Academic Free License with MIT Licence [CKR]
;;; 08/30/05 added license notice [CKR]
;;; 06/28/05 changed RUN-TESTS to compile code at run time, not expand time [CKR]
;;; 02/21/05 removed length check from SET-EQUAL [CKR]
;;; 02/17/05 added RUN-ALL-TESTS [CKR]
;;; 01/18/05 added ASSERT-EQUAL back in [CKR]
;;; 01/17/05 much clean up, added WITH-TEST-LISTENER [CKR] 
;;; 01/15/05 replaced ASSERT-EQUAL etc. with ASSERT-TRUE and ASSERT-FALSE [CKR]
;;; 01/04/05 changed COLLECT-RESULTS to echo output on *STANDARD-OUTPuT* [CKR]
;;; 01/04/05 added optional package argument to REMOVE-ALL-TESTS [CKR]
;;; 01/04/05 changed OUTPUT-OK-P to trim spaces and returns [CKR]
;;; 01/04/05 changed OUTPUT-OK-P to not check output except when asked to [CKR]
;;; 12/03/04 merged REMOVE-TEST into REMOVE-TESTS [CKR]
;;; 12/03/04 removed ability to pass forms to RUN-TESTS [CKR]
;;; 12/03/04 refactored RUN-TESTS expansion into RUN-TEST-THUNKS [CKR]
;;; 12/02/04 changed to group tests under packages [CKR]
;;; 11/30/04 changed assertions to put expected value first, like JUnit [CKR]
;;; 11/30/04 improved error handling and summarization [CKR]
;;; 11/30/04 generalized RUN-TESTS, removed RUN-TEST [CKR]
;;; 02/27/04 fixed ASSERT-PRINTS not ignoring value [CKR]
;;; 02/07/04 fixed ASSERT-EXPANDS failure message [CKR]
;;; 02/07/04 added ASSERT-NULL, ASSERT-NOT-NULL [CKR]
;;; 01/31/04 added error handling and totalling to RUN-TESTS [CKR]
;;; 01/31/04 made RUN-TEST/RUN-TESTS macros [CKR]
;;; 01/29/04 fixed ASSERT-EXPANDS quote bug [CKR]
;;; 01/28/04 major changes from BUG-FINDER to be more like JUnit [CKR]


#|
How to use
----------

1. Read the documentation in lisp-unit.html.

2. Make a file of DEFINE-TEST's. See exercise-tests.lisp for many
examples. If you want, start your test file with (REMOVE-TESTS) to
clear any previously defined tests.

2. Load this file.

2. (use-package :lisp-unit)

3. Load your code file and your file of tests.

4. Test your code with (RUN-TESTS test-name1 test-name2 ...) -- no quotes! --
or simply (RUN-TESTS) to run all defined tests.

A summary of how many tests passed and failed will be printed,
with details on the failures.

Note: Nothing is compiled until RUN-TESTS is expanded. Redefining
functions or even macros does not require reloading any tests.

For more information, see lisp-unit.html. 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage #:lisp-unit
  (:use #:common-lisp)
  (:export #:define-test #:run-all-tests #:run-tests
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-error #:assert-expands #:assert-false 
           #:assert-equality #:assert-prints #:assert-true
           #:get-test-code #:get-tests
           #:remove-all-tests #:remove-tests
           #:logically-equal #:set-equal
           #:use-debugger
           #:with-test-listener)
  )

(in-package #:lisp-unit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-listener* nil)

(defparameter *tests* (make-hash-table))

;;; Used by RUN-TESTS to collect summary statistics
(defvar *test-count* 0)
(defvar *pass-count* 0)

;;; Set by RUN-TESTS for use by SHOW-FAILURE
(defvar *test-name* nil)

;;; If nil, errors in tests are caught and counted.
;;; If :ask, user is given option of entering debugger or not.
;;; If true and not :ask, debugger is entered.
(defparameter *use-debugger* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINE-TEST

(defmacro define-test (name &body body)
  `(progn
     (store-test-code ',name ',body)
     ',name))

;;; ASSERT macros

(defmacro assert-eq (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
 (expand-assert :equal form form expected extras :test #'equalp))

(defmacro assert-error (condition form &rest extras)
 (expand-assert :error form (expand-error-form form)
                condition extras))

(defmacro assert-expands (&environment env expansion form &rest extras)
  (expand-assert :macro form 
                 (expand-macro-form form #+lispworks nil #-lispworks env)
                 expansion extras))

(defmacro assert-false (form &rest extras)
  (expand-assert :result form form nil extras))
 
(defmacro assert-equality (test expected form &rest extras)
 (expand-assert :equal form form expected extras :test test))

(defmacro assert-prints (output form &rest extras)
  (expand-assert :output form (expand-output-form form)
                 output extras))

(defmacro assert-true (form &rest extras)
  (expand-assert :result form form t extras))


(defun expand-assert (type form body expected extras &key (test #'eql))
  `(internal-assert
    ,type ',form #'(lambda () ,body) #'(lambda () ,expected) ,(expand-extras extras), test))
  
(defun expand-error-form (form)
  `(handler-case ,form
     (condition (error) error)))

(defun expand-output-form (form)
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output* (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defun expand-macro-form (form env)
  `(macroexpand-1 ',form ,env))

(defun expand-extras (extras)
  `#'(lambda ()
       (list ,@(mapcan #'(lambda (form) (list `',form form)) extras))))
    

;;; RUN-TESTS

(defmacro run-all-tests (package &rest tests)
  `(let ((*package* (find-package ',package)))
     (run-tests
      ,@(mapcar #'(lambda (test) (find-symbol (symbol-name test) package))
          tests))))

(defmacro run-tests (&rest names)
  `(run-test-thunks (get-test-thunks ,(if (null names) '(get-tests *package*) `',names))))

(defun get-test-thunks (names &optional (package *package*))
  (mapcar #'(lambda (name) (get-test-thunk name package))
    names))

(defun get-test-thunk (name package)
  (assert (get-test-code name package) (name package)
          "No test defined for ~S in package ~S" name package)
  (list name (coerce `(lambda () ,@(get-test-code name)) 'function)))

(defun use-debugger (&optional (flag t))
  (setq *use-debugger* flag))

;;; WITH-TEST-LISTENER
(defmacro with-test-listener (listener &body body)
  `(let ((*test-listener* #',listener)) ,@body))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-test-code (name &optional (package *package*))
  (let ((table (get-package-table package)))
    (unless (null table)
      (gethash name table))))

(defun get-tests (&optional (package *package*))
  (let ((l nil)
        (table (get-package-table package)))
    (cond ((null table) nil)
          (t
           (maphash #'(lambda (key val)
                        (declare (ignore val))
                        (push key l))
                    table)
           (sort l #'string< :key #'string)))))


(defun remove-tests (names &optional (package *package*))
  (let ((table (get-package-table package)))
    (unless (null table)
      (if (null names)
          (clrhash table)
        (dolist (name names)
          (remhash name table))))))

(defun remove-all-tests (&optional (package *package*))
  (if (null package)
      (clrhash *tests*)
    (remhash (find-package package) *tests*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DEFINE-TEST support

(defun get-package-table (package &key create)
  (let ((table (gethash (find-package package) *tests*)))
    (or table
        (and create
             (setf (gethash package *tests*)
                   (make-hash-table))))))

(defun get-test-name (form)
  (if (atom form) form (cadr form)))

(defun store-test-code (name code &optional (package *package*))
  (setf (gethash name
                 (get-package-table package :create t))
        code))


;;; ASSERTION support

(defun internal-assert (type form code-thunk expected-thunk extras test)
  (let* ((expected (multiple-value-list (funcall expected-thunk)))
         (actual (multiple-value-list (funcall code-thunk)))
         (passed (test-passed-p type expected actual test)))
    
    (incf *test-count*)
    (when passed
      (incf *pass-count*))
    
    (record-result passed type form expected actual extras)
    
    passed))

(defun record-result (passed type form expected actual extras)
  (funcall (or *test-listener* 'default-listener)
           passed type *test-name* form expected actual 
           (and extras (funcall extras))
           *test-count* *pass-count*))

(defun default-listener
    (passed type name form expected actual extras test-count pass-count)
  (declare (ignore test-count pass-count))
  (unless passed
    (show-failure type (get-failure-message type)
                  name form expected actual extras)))

(defun test-passed-p (type expected actual test)
  (ecase type
    (:error
     (or (eql (car actual) (car expected))
         (typep (car actual) (car expected))))
    (:equal
     (and (<= (length expected) (length actual))
          (every test expected actual)))
    (:macro
     (equal (car actual) (car expected)))
    (:output
     (string= (string-trim '(#\newline #\return #\space) 
                           (car actual))
              (car expected)))
    (:result
     (logically-equal (car actual) (car expected)))
    ))


;;; RUN-TESTS support

(defun run-test-thunks (test-thunks)
  (unless (null test-thunks)
    (let ((total-test-count 0)
          (total-pass-count 0)
          (total-error-count 0))
      (dolist (test-thunk test-thunks)
        (multiple-value-bind (test-count pass-count error-count)
            (run-test-thunk (car test-thunk) (cadr test-thunk))
          (incf total-test-count test-count)
          (incf total-pass-count pass-count)
          (incf total-error-count error-count)))
      (unless (null (cdr test-thunks))
        (show-summary 'total total-test-count total-pass-count total-error-count))
      (values))))

(defun run-test-thunk (*test-name* thunk)
  (if (null thunk)
      (format t "~&    Test ~S not found" *test-name*)
    (prog ((*test-count* 0)
           (*pass-count* 0)
           (error-count 0))
      (handler-bind 
          ((error #'(lambda (e)
                      (let ((*print-escape* nil))
                        (setq error-count 1)         
                        (format t "~&    ~S: ~W" *test-name* e))
                      (if (use-debugger-p e) e (go exit)))))
        (funcall thunk)
        (show-summary *test-name* *test-count* *pass-count*))
      exit
      (return (values *test-count* *pass-count* error-count)))))

(defun use-debugger-p (e)
  (and *use-debugger*
       (or (not (eql *use-debugger* :ask))
           (y-or-n-p "~A -- debug?" e))))

;;; OUTPUT support

(defun get-failure-message (type)
  (case type
    (:error "~&~@[Should have signalled ~{~S~^; ~} but saw~] ~{~S~^; ~}")
    (:macro "~&Should have expanded to ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (:output "~&Should have printed ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (t "~&Expected ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    ))

(defun show-failure (type msg name form expected actual extras)
  (format t "~&~@[~S: ~]~S failed: " name form)
  (format t msg expected actual)
  (format t "~{~&   ~S => ~S~}~%" extras)
  type)

(defun show-summary (name test-count pass-count &optional error-count)
  (format t "~&~A: ~S assertions passed, ~S failed~@[, ~S execution errors~]."
          name pass-count (- test-count pass-count) error-count))

(defun collect-form-values (form values)
  (mapcan #'(lambda (form-arg value)
              (if (constantp form-arg)
                  nil
                (list form-arg value)))
          (cdr form)
          values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful equality predicates for tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (LOGICALLY-EQUAL x y) => true or false
;;;   Return true if x and y both false or both true

(defun logically-equal (x y)
  (eql (not x) (not y)))

;;; (SET-EQUAL l1 l2 :test) => true or false
;;;   Return true if every element of l1 is an element of l2
;;;   and vice versa.

(defun set-equal (l1 l2 &key (test #'equal))
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))


(provide "lisp-unit")
