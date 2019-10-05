(cl:in-package #:sicl-cons)

;;; It used to be the case that the append function was defined in
;;; terms of (loop ... nconc ...), but it turns out that some
;;; implementations have buggy versions of (loop ... nconc ...)  so it
;;; is better to just define it in terms of a lower-level construct.
;;; Doing it this way allows for implementations with a buggy loop to
;;; still use this module without first replacing loop by ours.

(defun nconc (&rest lists)
  (if (null lists)
      nil
      ;; As with append, we use a sentinel to avoid a test in a loop. 
      (let* ((sentinel (list nil))
             ;; The variable last is a pointer to the last cell of the
             ;; accumulated list. 
             (last sentinel))
        (loop for remaining = lists then (cdr remaining)
              ;; Stop when we have processed every list except the last
              ;; (which may be any object). 
              until (null (cdr remaining))
              do (let ((list (car remaining)))
                   (cond ((consp list)
                          ;; This is the normal case, the list has at
                          ;; least one cons cell, but may be proper or
                          ;; dotted. 
                          ;; 
                          ;; Hook it up to the end of the accumulated
                          ;; list.
                          (setf (cdr last) list)
                          ;; And move to the next item, making sure
                          ;; that last is right behind list. 
                          (setf last list)
                          (setf list (cdr list))
                          ;; Continue this process until list reaches
                          ;; an atom.  What that happens, last points
                          ;; to the last cons cell of the list, which
                          ;; is exactly what we need for next
                          ;; iteration.
                          (loop while (consp list)
                                do (setf last list)
                                   (setf list (cdr list))))
                         ((null list)
                          ;; If the list is nil, we just skip it. 
                          nil)
                         (t
                          ;; It is neither a cons nor nil, so we
                          ;; signal an error. 
                          (error 'must-be-list
                                 :datum (car remaining)
                                 :name 'nconc))))
              finally (progn
                        ;; Hook up the last list to the end
                        ;; of what we have accumulated
                        (setf (cdr last) (car remaining))))
        ;; Skip the sentinel and return the rest. 
        (cdr sentinel))))
