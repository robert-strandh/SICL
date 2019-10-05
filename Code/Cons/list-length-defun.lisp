(cl:in-package #:sicl-cons)

;;; The standard requires the argument to be either a circular list
;;; or a proper list.  In case of a circular list, NIL should be 
;;; returned.  
(defun list-length (list)
  (cond ((consp list)
         ;; The list starts with a cons.  We use the method by which
         ;; one pointer (fast) advences by two steps in each
         ;; iteration, and another pointer (slow) advances by one
         ;; step.  If the two meet we have a circular list.  The only
         ;; purpose of slow is to detect circularity.  The length of
         ;; the list is determined by the number of cons cells seen by
         ;; fast.
         (let ((slow list)
               (fast (cdr list))
               ;; This variable counts the number of cons cells as
               ;; seen by fast.
               (length 1))
           (loop do (when (eq slow fast)
                      ;; We have a circular list
                      (return-from list-length nil))
                    ;; Start by checking whether fast is a cons,
                    ;; because this is the most likely case, so will
                    ;; likely succeed.
                    (cond ((consp fast)
                           ;; We are not at the end of the list
                           ;; try to advance by cddr, but do it in
                           ;; two steps so that we can check that
                           ;; we are allowed to do that.
                           (setf fast (cdr fast))
                           ;; We now need to remember that the value of
                           ;; length is one less that the number of cons
                           ;; cells seen by fast. 
                           (cond ((consp fast)
                                  ;; We indeed had two conses, so we can
                                  ;; make fast advance by one more cons.
                                  ;; This is the normal iteration case.
                                  (setf fast (cdr fast))
                                  ;; We advanced fast by cddr, so now we need to
                                  ;; advance slow by cdr. 
                                  (setf slow (cdr slow))
                                  ;; The length is counted by the
                                  ;; position of fast, so since we
                                  ;; advanced by cdr, the length
                                  ;; increases by 2. 
                                  (incf length 2))
                                 ((null fast)
                                  ;; We reached the end of a proper
                                  ;; list.  But we already advanced
                                  ;; fast by cdr without incrementing
                                  ;; length, so we need to compensate
                                  ;; for that.
                                  (return-from list-length (1+ length)))
                                 (t
                                  ;; The variable fast contains
                                  ;; neither a a cons cell nor nil.
                                  ;; This is a violation of the
                                  ;; requirement that the list given
                                  ;; should be either proper or
                                  ;; circular.
                                  (error 'must-be-proper-or-circular-list
                                         :datum list
                                         :name 'list-length))))
                          ((null fast)
                           ;; We reached the end of the list.
                           (return-from list-length length))
                          (t
                           ;; The variable fast contains
                           ;; neither a a cons cell nor nil.
                           ;; This is a violation of the
                           ;; requirement that the list given
                           ;; should be either proper or
                           ;; circular.
                           (error 'must-be-proper-or-circular-list
                                  :datum list
                                  :name 'list-length))))))
        ((null list)
         ;; The initial list was empty, and such a list has zero length
         0)
        (t
         ;; The argument given is neither a cons cell nor nil.  This
         ;; is a violation of the requirement that the list given
         ;; should be either proper or circular.
         (error 'must-be-proper-or-circular-list
                :datum list
                :name 'list-length))))
