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
(defun congruent-required-p
    (canonicalized-lambda-list-1 canonicalized-lambda-list-2)
  (= (length (extract-required canonicalized-lambda-list-1))
     (length (extract-required canonicalized-lambda-list-2))))

;;; Check rule number 2.
(defun congruent-optionals-p
    (canonicalized-lambda-list-1 canonicalized-lambda-list-2)
  (let ((optional1
          (extract-named-group canonicalized-lambda-list-1 '&optional))
        (optional2
          (extract-named-group canonicalized-lambda-list-2 '&optional)))
    (= (length optional1) (length  optional2))))

;;; Check rule number 3.
(defun congruent-key-rest-p
    (canonicalized-lambda-list-1 canonicalized-lambda-list-2)
  (let ((rest1
          (extract-named-group canonicalized-lambda-list-1 '&rest))
        (rest2
          (extract-named-group canonicalized-lambda-list-2 '&rest))
        (key1
          (extract-named-group canonicalized-lambda-list-1 '&key))
        (key2
          (extract-named-group canonicalized-lambda-list-2 '&key)))
    (or (and (null rest1) (null key1) (null rest2) (null key2))
        (and (or (not (null rest1)) (not (null key1)))
             (or (not (null rest2)) (not (null key2)))))))

(defun same-keys-accepted-p
    (canonicalized-generic-function-lambda-list canonicalized-method-lambda-list)
  (let ((generic-function-key
          (extract-named-group
           canonicalized-generic-function-lambda-list '&key)))
    (or (null generic-function-key)
        (let ((method-key
                (extract-named-group
                 canonicalized-method-lambda-list '&key))
              (method-allow-other-keys
                (extract-named-group
                 canonicalized-method-lambda-list '&allow-other-keys))
              (method-rest
                (extract-named-group
                 canonicalized-method-lambda-list '&allow-other-keys)))
          (or (not (null method-allow-other-keys))
              (and (not (null method-rest))
                   (null method-key))
              (loop with method-keywords = (mapcar #'caar (cdr method-key))
                    for key-parameter in (rest generic-function-key)
                    for keyword = (caar key-parameter)
                    always (member keyword method-keywords :test #'eq)))))))

(defun lambda-lists-congruent-p
    (canonicalized-generic-function-lambda-list canonicalized-method-lambda-list)
  (and (congruent-required-p
        canonicalized-generic-function-lambda-list
        canonicalized-method-lambda-list)
       (congruent-optionals-p
        canonicalized-generic-function-lambda-list
        canonicalized-method-lambda-list)
       (congruent-key-rest-p
        canonicalized-generic-function-lambda-list
        canonicalized-method-lambda-list)
       (same-keys-accepted-p
        canonicalized-generic-function-lambda-list
        canonicalized-method-lambda-list)))

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

(defun generate-congruent-lambda-list (canonicalized-method-lambda-list)
  (let ((required
          (extract-required canonicalized-method-lambda-list))
        (optional
          (extract-named-group canonicalized-method-lambda-list '&optional))
        (key
          (extract-named-group canonicalized-method-lambda-list '&key)))
    `(,@required
      ,@(if (null optional)
            '()
            `((&optional ,@(mapcar #'car optional))))
      ,@(if (null key)
            '()
            '((&key))))))
