(cl:in-package #:cleavir-code-utilities)

(defun first-group-is (remaining lambda-list-keyword)
  (and (not (null remaining))
       (not (null (first remaining)))
       (eq (first (first remaining)) lambda-list-keyword)))

(defun handle-required
    (required variable canonicalized-lambda-list invoking-form-variable)
  (let ((bindings '())
        (ignored-variables '())
        (not-enough-arguments-form
          `(error 'too-few-arguments
                  :lambda-list
                  ',(reduce #'append canonicalized-lambda-list)
                  :invoking-form ,invoking-form-variable)))
    (loop for pattern in required
          do (if (symbolp pattern)
                 (progn (push `(,pattern (if (null ,variable)
                                             ,not-enough-arguments-form
                                             (first ,variable)))
                              bindings)
                        (push `(,variable (rest ,variable))
                              bindings))
                 (let ((temp (gensym)))
                   (push `(,temp (if (null ,variable)
                                     ,not-enough-arguments-form
                                     (first ,variable)))
                         bindings)
                   (push `(,variable (rest ,variable))
                         bindings)
                   (multiple-value-bind
                         (nested-bindings nested-ignored-variables)
                       (new-destructure-lambda-list pattern temp invoking-form-variable)
                     (setf bindings
                           (append nested-bindings bindings))
                     (setf ignored-variables
                           (append nested-ignored-variables ignored-variables))))))
    (values bindings ignored-variables)))

(defun handle-optional (optional variable)
  (let ((bindings '()))
    (loop for (var default supplied-p) in optional
          do (unless (null supplied-p)
               (push `(,supplied-p (not (null ,variable)))
                     bindings))
             (push `(,var (if (null ,variable)
                              ,default
                              (first ,variable)))
                   bindings)
             (push `(,variable (if (null ,variable)
                                   ,variable
                                   (rest ,variable)))
                   bindings))
    bindings))

(defun handle-rest/body
    (pattern variable invoking-form-variable)
  (let ((bindings '())
        (ignored-variables '()))
    (if (symbolp pattern)
        (push `(,pattern ,variable) bindings)
        (let ((temp (gensym)))
          (push `(,temp ,variable)
                bindings)
          (multiple-value-bind (nested-bindings nested-ignored-variables)
              (new-destructure-lambda-list pattern temp invoking-form-variable)
            (setf bindings
                  (append nested-bindings bindings))
            (setf ignored-variables
                  (append nested-ignored-variables ignored-variables)))))
    (values bindings ignored-variables)))

(defun handle-key
    (key variable canonicalized-lambda-list invoking-form-variable allow-other-keys)
  (let* ((bindings '())
         (ignored-variables '())
         (keywords (mapcar #'caar key))
         (odd-number-of-keyword-arguments-form
           `(error 'odd-number-of-keyword-arguments
                   :lambda-list
                   ',(reduce #'append canonicalized-lambda-list)
                   :invoking-form ,invoking-form-variable))
         (check-keywords-form
           (let ((temp (gensym)))
             `(let ((,temp ,variable))
                (tagbody
                 again
                   (if (null ,temp) (go out))
                   (if (not (member (first ,temp)
                                    '(:allow-other-keys ,@keywords)
                                    :test #'eq))
                       (error 'invalid-keyword
                              :keyword (first ,temp)
                              :lambda-list
                              ',(reduce #'append canonicalized-lambda-list)
                              :invoking-form ,invoking-form-variable)
                       (progn (setf ,temp (cddr ,temp))
                              (go again)))
                 out)))))
    (let ((ignored (gensym)))
      (push ignored ignored-variables)
      (push `(,ignored (if (oddp (length ,variable))
                           ,odd-number-of-keyword-arguments-form))
            bindings))
    (unless allow-other-keys
      (let ((ignored (gensym)))
        (push ignored ignored-variables)
        (push `(,ignored (if (not (getf ,variable :allow-other-keys))
                             ,check-keywords-form))
              bindings)))
    (loop for ((keyword var) default supplied-p) in key
          for temp1 = (gensym)
          for temp2 = (gensym)
          do (push `(,temp1 (list nil)) bindings)
             (push `(,temp2 (getf ,variable ,keyword ,temp1))
                   bindings)
             (if (null supplied-p)
                 nil
                 (push `(,supplied-p (not (eq ,temp2 ,temp1)))
                       bindings))
             (push `(,var (if (eq ,temp2 ,temp1)
                              ,default
                              ,temp2))
                   bindings))
    (values bindings ignored-variables)))

(defun new-destructure-lambda-list
    (canonicalized-lambda-list variable invoking-form-variable)
  (let* ((remaining canonicalized-lambda-list)
         (bindings '())
         (ignored-variables '()))
    (unless (or (null remaining)
                (member (first (first remaining)) (intrinsic-keywords)
                        :test #'eq))
      (multiple-value-bind (nested-bindings nested-ignored-variables)
          (handle-required
           (pop remaining) variable canonicalized-lambda-list invoking-form-variable)
        (setf bindings
              (append nested-bindings bindings))
        (setf ignored-variables
              (append nested-ignored-variables ignored-variables))))
    (when (first-group-is remaining '&optional)
      (setf bindings
            (append (handle-optional (rest (pop remaining)) variable)
                    bindings)))
    (unless (or (member '&rest remaining :key #'first :test #'eq)
                (member '&body remaining :key #'first :test #'eq)
                (member '&key remaining :key #'first :test #'eq))
      (let ((temp (gensym)))
        (push temp ignored-variables)
        (push `(,temp (if (not (null ,variable))
                          (error 'too-many-arguments
                                 :lambda-list
                                 ',(reduce #'append canonicalized-lambda-list)
                                 :invoking-form ,invoking-form-variable)))
              bindings)))
    (when (or (first-group-is remaining '&rest)
              (first-group-is remaining '&body))
      (multiple-value-bind (nested-bindings nested-ignored-variables)
          (handle-rest/body
           (second (pop remaining)) variable invoking-form-variable)
        (setf bindings
              (append nested-bindings bindings))
        (setf ignored-variables
              (append nested-ignored-variables ignored-variables))))
    (when (first-group-is remaining '&key)
      (let* ((group (pop remaining))
             (allow-other-keys
               (if (first-group-is remaining '&allow-other-keys)
                   (progn (pop remaining) t)
                   nil)))
        (multiple-value-bind (nested-bindings nested-ignored-variables)
            (handle-key
             (rest group)
             variable
             canonicalized-lambda-list
             invoking-form-variable
             allow-other-keys)
          (setf bindings
                (append nested-bindings bindings))
          (setf ignored-variables
                (append nested-ignored-variables ignored-variables)))))
    (when (first-group-is remaining '&aux)
      (setf bindings
            (append (reverse (rest (pop remaining))) bindings)))
    (values bindings ignored-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSE-MACRO
;;;
;;; According to CLtL2.

(defun parse-macro (name lambda-list body &optional environment)
  (declare (ignore name environment)) ; For now.
  (let* ((canonicalized-lambda-list
           (canonicalize-macro-lambda-list lambda-list))
         (environment-group
           (extract-named-group canonicalized-lambda-list '&environment))
         (environment-parameter
           (if (null environment-group) (gensym) (second environment-group)))
         (whole-group
           (extract-named-group canonicalized-lambda-list '&whole))
         (whole-parameter
           (if (null whole-group) (gensym) (second whole-group)))
         (remaining
           (remove '&environment
                   (remove '&whole canonicalized-lambda-list
                           :key #'first :test #'eq)
                   :key #'first :test #'eq))
         (args-var (gensym)))
    (multiple-value-bind (declarations documentation forms)
        (separate-function-body body)
      (multiple-value-bind (bindings ignored-variables)
          (new-destructure-lambda-list remaining args-var whole-parameter)
        `(lambda (,whole-parameter ,environment-parameter)
           ,@(if (null documentation) '() (list documentation))
           ;; If the lambda list does not contain &environment, then
           ;; we IGNORE the GENSYMed parameter to avoid warnings.
           ;; If the lambda list does contain &environment, we do
           ;; not want to make it IGNORABLE because we would want a
           ;; warning if it is not used then.
           ,@(if (null environment-group)
                 `((declare (ignore ,environment-parameter)))
                 `())
           (let ((,args-var (rest ,whole-parameter)))
             (let* ,(reverse bindings)
               (declare (ignore ,@ignored-variables))
               ,@declarations
               ,@forms)))))))
