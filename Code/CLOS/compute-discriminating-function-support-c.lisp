(cl:in-package #:sicl-clos)

(defun make-discriminating-function-lambda (generic-function)
  (let* ((specializer-profile (specializer-profile generic-function))
         ;; We do not use the Common Lisp function COUNT here, because
         ;; we might want to define it as a generic function.
         (active-arg-count (loop for x in specializer-profile
                                 count x))
         (class-number-vars (loop for x in specializer-profile
                                  when x collect (gensym)))
         (call-history (call-history generic-function)))
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
            for effective-method = (effective-method-cache call-cache)
            do (when (null (assoc effective-method dico :test #'eq))
                 (push (cons effective-method
                             `(return-from b
                                (funcall ,effective-method arguments)))
                       dico)))
      ;; Create a discriminating automaton with the entries in the call
      ;; history.
      (let ((automaton (make-automaton (1+ active-arg-count))))
        (loop for call-cache in call-history
              for active-classes = (class-cache call-cache)
              for effective-method = (effective-method-cache call-cache)
              for action = (cdr (assoc effective-method dico :test #'eq))
              do (add-path automaton active-classes action))
        (let* ((info (extract-transition-information automaton))
               (tagbody (compute-discriminating-tagbody info class-number-vars)))
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
                  ',specializer-profile)))))))))

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
