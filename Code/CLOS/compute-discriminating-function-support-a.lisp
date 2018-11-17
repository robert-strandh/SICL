(cl:in-package #:sicl-clos)

;;; In this version of COMPUTE-DISCRIMINATING-FUNCTION-DEFAULT, we DO
;;; use the compiler.  For a version that does not use the compiler,
;;; see the file compute-discriminating-function-support-b.lisp.

(defun make-discriminating-function-lambda (generic-function)
  (let* ((specializer-profile (specializer-profile generic-function))
         ;; We do not use the Common Lisp function COUNT here, because
         ;; we might want to define it as a generic function.
         (active-arg-count (loop for x in specializer-profile
                                 count x))
         (lambda-list (generic-function-lambda-list generic-function))
         ;; This variable is true if and only if this generic function
         ;; has only required parameters.
         (only-required-p
           (null (intersection lambda-list lambda-list-keywords)))
         (class-number-vars (loop for x in specializer-profile
                                  when x collect (gensym)))
         (call-history (call-history generic-function)))
    ;; Check for the special case when the active-arg-count is 0,
    ;; meaning that every method is unspecialized, including when
    ;; there are no methods at all.  Either way, each method is always
    ;; applicable.
    (when (zerop active-arg-count)
      ;; When every method is unspecialized, all methods are equally
      ;; specific, so they are already sorted by specificity.
      ;; Therefore, they are already in the order that would be
      ;; returned by COMPUTE-APPLICABLE-METHODS, so there is no need
      ;; to call that function.
      (let* ((methods (generic-function-methods generic-function))
             ;; We need the method combination of the generic
             ;; function, so that we can compute the effective method.
             (mc (generic-function-method-combination generic-function))
             ;; Submit that list to compute-effective-method, giving
             ;; us an effective method that should always be invoked.
             ;; Notice that we designed COMPUTE-EFFECTIVE-METHOD so
             ;; that it will not signal an error if there is no
             ;; primary method, but instead it will return an
             ;; effective method that signals an error when invoked.
             (effective-method
               (compute-effective-method generic-function mc methods)))
      (return-from make-discriminating-function-lambda
        (if only-required-p
            `(lambda ,lambda-list
               (funcall ,effective-method ,@lambda-list))
            `(lambda (&rest arguments)
               (apply ,effective-method arguments))))))
    ;; Check for the special case when the call history is empty.  In
    ;; that case, we just generate a call to the default
    ;; discriminating function.
    (when (null call-history)
      (return-from make-discriminating-function-lambda
        `(lambda (&rest arguments)
           (default-discriminating-function ,generic-function
                                            arguments
                                            ',specializer-profile))))
    ;; Come here when there is at least one active argument, i.e. at
    ;; least one element T in the specializer profile, AND the call
    ;; history is not empty.  Create a dictionary, mapping effective
    ;; methods to forms containing APPLY that call those methods.
    (let ((dico '()))
      (loop for call-cache in call-history
            for class-number-cache = (class-number-cache call-cache)
            for effective-method = (effective-method-cache call-cache)
            do (when (null (assoc effective-method dico :test #'eq))
                 (push (cons effective-method
                             `(return-from b
                                ,(if only-required-p
                                     `(funcall ,effective-method ,@lambda-list)
                                     `(apply ,effective-method arguments))))
                       dico)))
      ;; Create a discriminating automaton with the entries in the call
      ;; history.
      (let ((automaton (make-automaton (1+ active-arg-count))))
        (loop for call-cache in call-history
              for class-number-cache = (class-number-cache call-cache)
              for effective-method = (effective-method-cache call-cache)
              for action = (cdr (assoc effective-method dico :test #'eq))
              do (add-path automaton class-number-cache action))
        (let* ((info (extract-transition-information automaton))
               (tagbody (compute-discriminating-tagbody info class-number-vars)))
          (if only-required-p
              `(lambda ,lambda-list
                 (block b
                   (let ,(loop with i = 0
                               for x in specializer-profile
                               for arg in lambda-list
                               when x
                                 collect `(,(nth i class-number-vars)
                                           (stamp ,arg))
                                 and do (incf i))
                     ,tagbody
                     (default-discriminating-function
                      ,generic-function
                      (list ,@lambda-list)
                      ',specializer-profile))))
              `(lambda (&rest arguments)
                 (block b
                   (let ,(loop with i = 0
                               for x in specializer-profile
                               for j from 0
                               when x
                                 collect `(,(nth i class-number-vars)
                                           (stamp (nth ,j arguments)))
                                 and do (incf i))
                     ,tagbody
                     (default-discriminating-function
                      ,generic-function
                      arguments
                      ',specializer-profile))))))))))

;;; This function takes a generic function an returns a discriminating
;;; function for it that has the GENERIC-FUNCTION argument compiled in
;;; as a constant, so that the discriminating function can pass the
;;; generic function to the default discriminating function.
(defun make-default-discriminating-function (generic-function)
  (compile
   nil
   (make-discriminating-function-lambda generic-function)))

(defun compute-discriminating-function-default (generic-function)
  (make-default-discriminating-function generic-function))
