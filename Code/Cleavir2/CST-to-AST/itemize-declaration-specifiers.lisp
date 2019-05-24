(cl:in-package #:cleavir-cst-to-ast)

;;;; Recall that a canonical declaration specifier is an ordinary
;;;; Common Lisp list containing two CSTs, the first one representing
;;;; a declaration identifier and the second one representing a single
;;;; declaration datum such as the name of a variable.

;;; Separate a list of canonical declaration specifiers into two
;;; disjoint sets, returned as two values.  The first set contains All
;;; the declerations specifiers that concern an ordinary variable
;;; named NAME, and the second set the remaining declaration
;;; specifiers.
(defun separate-declarations (canonical-declaration-specifiers name-cst)
  (loop with name = (cst:raw name-cst)
        for spec in canonical-declaration-specifiers
        for declaration-identifier = (cst:raw (cst:first spec))
        if (or (and (member declaration-identifier
                            '(ignore ignorable dynamic-extent special))
                    (eq (cst:raw (cst:second spec)) name))
               (and (eq declaration-identifier 'type)
                    (eq (cst:raw (cst:third spec)) name)))
          collect spec into first
        else
          collect spec into second
        finally (return (values first second))))

;;; This function takes two arguments.  The first argument, VARIABLES,
;;; is a list of items, where each item is a non-empty list of CSTs.
;;; The CSTs in an item represent the variables that are bound in a
;;; single binding in a lambda list.  The second argument,
;;; CANONICAL-DSPECS, is a list of canonical declaration specifiers.
;;; This function returns a two values.  The first return value is a
;;; list with the same length as VARIABLES.  Each element in that list
;;; contains the elements in CANONICAL-DSPECS that apply to (all) the
;;; variables in the corresponding item in VARIABLES.  The second
;;; return value is a list of the remaining declaration specifiers in
;;; CANONICAL-DSPECS i.e. the ones that do not apply to any element in
;;; VARIABLES.  A particular symbol S can not appear twice in an item
;;; of VARIABLES, but it can appear in different items.  In that case,
;;; the declaration specifiers that apply to that symbol will be
;;; associated with the last item in the list of VARIABLES.
(defun itemize-declaration-specifiers (variables canonical-dspecs)
  (if (null variables)
      (values '() canonical-dspecs)
      (multiple-value-bind (itemized-dspecs remaining-dspecs)
          (itemize-declaration-specifiers (cdr variables) canonical-dspecs)
        (let ((item-specific-dspecs '()))
          (loop for var in (first variables)
                do (multiple-value-bind (is-dspecs r-dspecs)
                       (separate-declarations remaining-dspecs var)
                     (setf item-specific-dspecs
                           (append is-dspecs item-specific-dspecs))
                     (setf remaining-dspecs r-dspecs)))
          (values (cons item-specific-dspecs itemized-dspecs)
                  remaining-dspecs)))))
