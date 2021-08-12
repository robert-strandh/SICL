(cl:in-package #:cleavir-code-utilities)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Check that a lambda list of a geneneric function and a
;;; lambda list from a method are congruent.
;;;
;;; The CLHS section 7.6.4 says that:
;;;
;;; 1. Each lambda list must have the same number of required
;;;    parameters.
;;;
;;; 2. Each lambda list must have the same number of optional
;;;    parameters.  Each method can supply its own default for an
;;;    optional parameter.
;;;
;;; 3. If any lambda list mentions &rest or &key, each lambda list
;;;    must mention one or both of them.
;;;
;;; 4. If the generic function lambda list mentions &key, each method
;;;    must accept all of the keyword names mentioned after &key,
;;;    either by accepting them explicitly, by specifying
;;;    &allow-other-keys, or by specifying &rest but not &key. Each
;;;    method can accept additional keyword arguments of its own. The
;;;    checking of the validity of keyword names is done in the
;;;    generic function, not in each method. A method is invoked as if
;;;    the keyword argument pair whose name is :allow-other-keys and
;;;    whose value is true were supplied, though no such argument pair
;;;    will be passed.
;;;
;;; 5. The use of &allow-other-keys need not be consistent across
;;;    lambda lists. If &allow-other-keys is mentioned in the lambda
;;;    list of any applicable method or of the generic function, any
;;;    keyword arguments may be mentioned in the call to the generic
;;;    function.
;;;
;;; 6. The use of &aux need not be consistent across methods.

;;; Check rule number 1.
(defun congruent-required-p (lambda-list-1 lambda-list-2)
  (= (length (required lambda-list-1))
     (length (required lambda-list-2))))

;;; Check rule number 2.
(defun congruent-optionals-p (lambda-list-1 lambda-list-2)
  (or (and (eq (optionals lambda-list-1) :none)
           (eq (optionals lambda-list-2) :none))
      (and (listp (optionals lambda-list-1))
           (listp (optionals lambda-list-2))
           (= (length (optionals lambda-list-1))
              (length (optionals lambda-list-2))))))

;;; Check rule number 3.
(defun congruent-key-rest-p (lambda-list-1 lambda-list-2)
  (or (and (eq (rest-body lambda-list-1) :none)
           (eq (keys lambda-list-1) :none)
           (eq (rest-body lambda-list-2) :none)
           (eq (keys lambda-list-2) :none))
      (and (or (not (eq (rest-body lambda-list-1) :none))
               (not (eq (keys lambda-list-1) :none)))
           (or (not (eq (rest-body lambda-list-2) :none))
               (not (eq (keys lambda-list-2) :none))))))

(defun same-keys-accepted-p
    (generic-function-lambda-list method-lambda-list)
  (or (eq (keys generic-function-lambda-list) :none)
      (null (set-exclusive-or
             (keys generic-function-lambda-list)
             (keys method-lambda-list)
             :test #'eq
             :key #'caar))
      (not (eq (allow-other-keys method-lambda-list) :none))
      (and (not (eq (rest-body method-lambda-list) :none))
           (eq (keys method-lambda-list) :none))))

(defun lambda-lists-congruent-p
    (generic-function-lambda-list method-lambda-list)
  (and (congruent-required-p
        generic-function-lambda-list method-lambda-list)
       (congruent-optionals-p
        generic-function-lambda-list method-lambda-list)
       (congruent-key-rest-p
        generic-function-lambda-list method-lambda-list)
       (same-keys-accepted-p
        generic-function-lambda-list method-lambda-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create a generic function lambda list that is congruent with a
;;; given method lambda list.
;;;
;;; The CLHS section 7.6.4 says that:
;;;
;;; If a method-defining operator that cannot specify generic function
;;; options creates a generic function, and if the lambda list for the
;;; method mentions keyword arguments, the lambda list of the generic
;;; function will mention &key (but no keyword arguments).
;;;
;;; We return 2 values, the unparsed and the parsed lambda list.

(defun generate-congruent-lambda-list (method-lambda-list)
  (let* ((parsed-lambda-list
           (make-instance
            'lambda-list
            :required (required method-lambda-list)
            :optionals (optionals method-lambda-list)
            :rest (rest-body method-lambda-list)
            :keys (if (eq (keys method-lambda-list) :none)
                      :none
                      '())))
         (unparsed-lambda-list
           `(,(required parsed-lambda-list)
             ,@(if (eq (optionals parsed-lambda-list) :none)
                   '()
                   `(&optional ,@(optionals parsed-lambda-list)))
             ,@(if (eq (rest-body parsed-lambda-list) :none)
                   '()
                   `(&rest ,@(rest-body parsed-lambda-list)))
             ,@(if (eq (keys parsed-lambda-list) :none)
                   '()
                   `(&key)))))
    (values unparsed-lambda-list
            parsed-lambda-list)))
