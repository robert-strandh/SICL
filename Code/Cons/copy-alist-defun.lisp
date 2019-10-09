(cl:in-package #:sicl-cons)

;;; The HyperSpec says that the argument is an alist, and in those
;;; cases, whenever the type doesn't correspond, it is legitimate to
;;; signal an error.  However, for copy-alist, the HyperSpec also says
;;; that any object that is referred to directly or indirectly is
;;; still shared betwee the argument and the resulting list, which
;;; suggests that any element that is not a cons should just be
;;; included in the resulting list as well.  And that is what we are
;;; doing.

;;; We use (loop for ... on ...) because it uses ATOM to test for the
;;; end of the list.  Then we check that the atom is really null, and
;;; if not, signal a type error.

(defun copy-alist (alist)
  (loop for remaining on alist
        collect (if (consp (car remaining))
                    (cons (caar remaining) (cdar remaining))
                    (car remaining))
        finally (unless (null remaining)
                  (error 'must-be-proper-list
                         :datum alist
                         :name 'copy-alist))))
