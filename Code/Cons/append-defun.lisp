(cl:in-package #:sicl-cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function append

;;; It used to be the case that the append function was defined
;;; in terms of (loop ... append ...), but it turns out that
;;; some implementations have buggy versions of (loop ... append ...) 
;;; so it is better to just define it in terms of a lower-level
;;; construct.  Doing it this way allows for implementations with 
;;; a buggy loop to still use this module without first replacing
;;; loop by ours. 

(defun append (&rest lists)
  (if (null lists)
      ;; The standard doesn't say explicity that calling append with
      ;; zero arguments is allowed and returns nil, but one of the
      ;; examples on the HyperSpec page for append suggests that. 
      '()
      ;; We need to copy every list except the last one.  Also,
      ;; every list except the last one must be a proper list.  In
      ;; order to avoid making a test in each iteration of the loop,
      ;; we use a sentinel, which is a cons cell whose cdr contains
      ;; the real result that we return in the end. 
      (let* ((sentinel (list nil))
             ;; The variable last points to the last cons cell of the
             ;; resulting list to be accumulated. 
             (last sentinel))
        (loop until (null (cdr lists))
              do (loop with list = (car lists)
                       do (cond ((consp list)
                                 ;; There are more cells, copy the first one.
                                 (setf (cdr last) (list (car list)))
                                 (setf last (cdr last))
                                 (setf list (cdr list)))
                                ((null list)
                                 ;; The list is a proper list and we reached the
                                 ;; end of it.
                                 (loop-finish))
                                (t
                                 ;; The list is a dotted list
                                 (error 'must-be-proper-list
                                        :datum (car lists)
                                        :name 'append))))
                 ;; We are through with the first list in lists
                 (setf lists (cdr lists)))
        ;; When we get here, there is only one list left in lists.
        ;; And in fact, it doesn't have to be a list at all. 
        ;; Attach it to the end of what we have accumulated so far.
        (setf (cdr last) (car lists))
        ;; Skip the sentinel and return the rest. 
        (cdr sentinel))))
