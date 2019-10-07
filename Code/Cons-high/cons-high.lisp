(cl:in-package :sicl-cons-high)

;;;; Copyright (c) 2008 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

;;; special version of last used when the second argument to
;;; last is 1. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last-1 (list)
    (unless (typep list 'list)
      (error 'must-be-list
	     :datum list
	     :name 'last))
    ;; We can use for ... on, because it uses atom to test for
    ;; the end of the list. 
    (loop for rest on list
	  do (setf list rest))
    list))

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nreconc

(defun nreconc (list tail)
  (loop with remaining = list
        with result = tail
        until (atom remaining)
        do (let ((temp (cdr remaining)))
             (setf (cdr remaining) result
                   result remaining
                   remaining temp))
        finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum list
			 :name 'nreconc))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function butlast

;;; The HyperSpec doesn't specify what happens if n is 0.
;;; There is a note that says:
;;;
;;;     (butlast list n) ==  (ldiff list (last list n))
;;;
;;; but notes are not normative.  If you do believe this note,
;;; then (butlast '(1 2 3 . 4) 0) should return (1 2 3), i.e., 
;;; it doesn't return the unmodified list.  
;;;
;;; This implementation works according to the HyperSpec note
;;; when n is 0.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun butlast (list &optional (n 1))
    (unless (typep list 'list)
      (error 'must-be-list
	     :datum list
	     :name 'butlast))
    (unless (typep n '(integer 0))
      (error 'must-be-nonnegative-integer
	     :datum n
	     :name 'butlast))
    (let ((remaining list))
      (loop repeat n
	    until (atom remaining)
	    do (setf remaining (cdr remaining)))
      (loop for slow on remaining
	    until (atom remaining)
	    do (setf remaining (cdr remaining))
	    collect (prog1 (car list) (setf list (cdr list)))
	    until (atom remaining)
	    do (setf remaining (cdr remaining))
	    collect (prog1 (car list) (setf list (cdr list)))
	    do (when (eq slow remaining)
		 ;; we have a circular list
		 (error 'must-be-proper-or-dotted-list
			:datum list
			:name 'butlast))))))

;;; There is probably no point in making a special version of 
;;; butlast for n = 1, because the time is going to be dominated
;;; by consing up the new list anyawy. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nbutlast

;;; Special version for n = 1.  This version avoids that 
;;; the list be traversed several times, and since list traversal
;;; dominates the time here, this is a win.

(defun nbutlast-1 (list)
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (if (atom (cdr list))
      nil
      ;; The compiler probably isn't smart enough to eliminate
      ;; common subexpressions such as (cdr x), so we do it
      ;; manually by only doing a single cdr in each iteration,
      ;; and by storing pointers to the result in local variables.
      (let ((a list)
            (b (cdr list))
            (c (cddr list)))
        (loop for slow on list
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      until (atom c)
              do (setf a b
                       b c
                       c (cdr c))
	      do (when (eq slow c)
		   ;; we have a circular list
		   (error 'must-be-proper-or-dotted-list
			  :datum list
			  :name 'nbutlast)))
	(setf (cdr a) nil)
        list)))

(defun nbutlast (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
	   :datum list
	   :name 'nbutlast))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
	   :datum n
	   :name 'nbutlast))
  (if (= n 1)
      (nbutlast-1 list)
      (let ((length (loop for slow on list
			  with conses = list
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
                          until (atom conses)
                          count t
			  do (setf conses (cdr conses))
			  do (when (eq slow conses)
			       ;; we have a circular list
			       (error 'must-be-proper-or-dotted-list
				      :datum list
				      :name 'nbutlast)))))
        (if (<= length n)
            nil
            (progn (setf (cdr (nthcdr (- length (1+ n)) list)) nil)
                   list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst

(defun |subst key=identity test=eq| (new old tree)
  (labels ((traverse (tree)
             (cond ((eq tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=identity test=eql| (new old tree)
  (labels ((traverse (tree)
             (cond ((eql tree old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test=eq| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test=eql| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key tree) old) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=identity test=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other-test-other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

;;; We do not provide special versions for when the key is identity and
;;; the test-not is either eq or eql.  Here is why:
;;;
;;; Let's start with eq, and let's suppose the old item is an atom.
;;; Then either the old item is eq to the tree or not eq to the tree.
;;; If the old item is eq to the tree, then the tree too is an atom,
;;; and it is returned, because no substitutions are made, and we
;;; reached a leaf of the tree.  If the old item is not eq to the
;;; tree, then we make a substitution and return the new item.  In
;;; other words, this operation is the same as:
;;; (if (eq old-item tree) old-item new-item)
;;;
;;; Now suppose the old item is not an atom, and thus a cons.  Then
;;; the old item is either eq to the tree or not.  If the old item is
;;; not eq to the tree, then we return the new item and stop.  If the
;;; old item is eq to the tree, consider the two immediate subtrees
;;; (car and cdr) of the tree.  If none of them is eq to the the old
;;; item, then we return a cons where the car and cdr both contain the
;;; new item.  Now let's say that the car of the tree is eq to the old
;;; item.  Then it is also eq to the tree itself (beacuase we are
;;; handling the case where the old item and the tree are eq).  Then
;;; we have a circular structure, and it is undefined (the Hyperspec
;;; actually doesn't say that, but the Notes on the relevant page
;;; suggest it) what happens.  It is therefore very unlikely that
;;; someone would use this special case, so we have decided not to
;;; include it.
;;;
;;; A similar argument can be made for eql. 

;;; While it is possible that a test-not of eq or eql could be useful
;;; in combination with a key argument that is not identity, we do not
;;; think that case is very common either, so we omit it too. 

(defun |subst key=identity test-not=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst key=other test-not=other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst (new old tree
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'subst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|subst key=other test=eq| new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|subst key=other test=eql| new old tree key)
                  (|subst key=other-test-other| new old tree test key)))
          (if test-not-given
	      (|subst key=other test-not=other| new old tree test-not key)
              (|subst key=other test=eql| new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|subst key=identity test=eq| new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|subst key=identity test=eql| new old tree)
                  (|subst key=identity test=other| new old tree test)))
          (if test-not-given
	      (|subst key=identity test-not=other| new old tree test-not)
              (|subst key=identity test=eql| new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if

(defun |subst-if key=other=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate tree) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst-if key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if (new predicate tree &key key)
  (if (null key)
      (|subst-if key=other=identity| new predicate tree)
      (|subst-if key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subst-if-not

(defun |subst-if-not key=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate tree)) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun |subst-if-not key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key tree))) new)
                   ((atom tree) tree)
                   (t (cons (traverse (car tree)) (traverse (cdr tree)))))))
    (traverse tree)))

(defun subst-if-not (new predicate tree &key key)
  (if (null key)
      (|subst-if-not key=identity| new predicate tree)
      (|subst-if-not key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst

(defun |nsubst key=identity test=eq| (new old tree)
  (labels ((traverse (tree)
             (cond ((eq (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=identity test=eql| (new old tree)
  (labels ((traverse (tree)
             (cond ((eql (car tree) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (cdr tree) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql tree old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test=eq| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eq (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eq (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eq (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test=eql| (new old tree key)
  (labels ((traverse (tree)
             (cond ((eql (funcall key (car tree)) old)
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((eql (funcall key (cdr tree)) old)
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((eql (funcall key tree) old) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=identity test=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((funcall test old (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other-test-other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((funcall test old (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall test old (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall test old (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

;;; As with subst, we do not provide special versions for a :test-not
;;; of eq or eql.  See comment above for an explanation.

(defun |nsubst key=identity test-not=other| (new old tree test)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst key=other test-not=other| (new old tree test key)
  (labels ((traverse (tree)
             (cond ((not (funcall test old (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall test old (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall test old (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst (new old tree
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nsubst))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsubst key=other test=eq| new old tree key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsubst key=other test=eql| new old tree key)
                  (|nsubst key=other-test-other|  new old tree test key)))
          (if test-not-given
	      (|nsubst key=other test-not=other|  new old tree test-not key)
              (|nsubst key=other test=eql| new old tree key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|nsubst key=identity test=eq| new old tree)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|nsubst key=identity test=eql| new old tree)
                  (|nsubst key=identity test=other|  new old tree test)))
          (if test-not-given
	      (|nsubst key=identity test-not=other|  new old tree test-not)
              (|nsubst key=identity test=eql| new old tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if

(defun |nsubst-if key=other=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (car tree))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (cdr tree))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate tree) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst-if key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((funcall predicate (funcall key (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((funcall predicate (funcall key (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((funcall predicate (funcall key tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if (new predicate tree &key key)
  (if (null key)
      (|nsubst-if key=other=identity| new predicate tree)
      (|nsubst-if key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubst-if-not

(defun |nsubst-if-not key=identity| (new predicate tree)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (car tree)))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (cdr tree)))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate tree)) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun |nsubst-if-not key=other| (new predicate tree key)
  ;; Define a local function so as to avoid passing the new and
  ;; predicate arguments to each recursive call.
  (labels ((traverse (tree)
             (cond ((not (funcall predicate (funcall key (car tree))))
                    (setf (car tree) new))
                   ((atom (car tree))
                    nil)
                   (t
                    (traverse (car tree))))
             (cond ((not (funcall predicate (funcall key (cdr tree))))
                    (setf (cdr tree) new))
                   ((atom (cdr tree))
                    nil)
                   (t
                    (traverse (cdr tree))))))
    (cond ((not (funcall predicate (funcall key tree))) new)
          ((atom tree) tree)
          (t (traverse tree) tree))))

(defun nsubst-if-not (new predicate tree &key key)
  (if (null key)
      (|nsubst-if-not key=identity| new predicate tree)
      (|nsubst-if-not key=other| new predicate tree key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some helper macros

(defmacro with-proper-list-rests ((rest-var list function-name) &body body)
  (let ((list-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

(defmacro with-proper-list-elements ((element-var list function-name) &body body)
  (let ((list-var (gensym))
	(rest-var (gensym)))
    `(loop with ,list-var = ,list
	   for ,rest-var on ,list-var
	   for ,element-var = (car ,rest-var)
	   do ,@body
	   finally (unless (null ,rest-var)
		     (error 'must-be-proper-list
			    :datum ,list-var
			    :name ,function-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member

(defun |member test=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eq item (car rest))
      (return rest))))

(defun |member test=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eq item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eq key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (car rest)))
      (return rest))))

(defun |member test-not=eq key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eq item (funcall key (car rest))))
      (return rest))))

(defun |member test=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (eql item (car rest))
      (return rest))))

(defun |member test=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (eql item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=eql key=identity| (name item list)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (car rest)))
      (return rest))))

(defun |member test-not=eql key=other| (name item list key)
  (with-proper-list-rests (rest list name)
    (when (not (eql item (funcall key (car rest))))
      (return rest))))

(defun |member test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (car rest))
      (return rest))))

(defun |member test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test item (funcall key (car rest)))
      (return rest))))

(defun |member test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (car rest)))
      (return rest))))

(defun |member test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test item (funcall key (car rest))))
      (return rest))))

(defun member (item list
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'member))
  (if key
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=other| 'member item list key)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=other| 'member item list key)
                  (|member test=other key=other| 'member item list test key)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=other| 'member item list key)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=other| 'member item list key)
                      (|member test-not=other key=other| 'member item list test-not key)))
              (|member test=eql key=other| 'member item list key)))
      (if test-given
          (if (or (eq test #'eq) (eq test 'eq))
              (|member test=eq key=identity| 'member item list)
              (if (or (eq test #'eql) (eq test 'eql))
                  (|member test=eql key=identity| 'member item list)
                  (|member test=other key=identity| 'member item list test)))
          (if test-not-given
              (if (or (eq test-not #'eq) (eq test-not 'eq))
                  (|member test-not=eq key=identity| 'member item list)
                  (if (or (eq test-not #'eql) (eq test-not 'eql))
                      (|member test-not=eql key=identity| 'member item list)
                      (|member test-not=other key=identity| 'member item list test-not)))
              (|member test=eql key=identity| 'member item list)))))

;;; Special versions of the member function with the arguments
;;; of the test reversed.

(defun |member reversed test=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (funcall test (car rest) item)
      (return rest))))

(defun |member reversed test=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (funcall test (funcall key (car rest)) item)
      (return rest))))

(defun |member reversed test-not=other key=identity| (name item list test)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (car rest) item))
      (return rest))))

(defun |member reversed test-not=other key=other| (name item list test key)
  (with-proper-list-rests (rest list name)
    (when (not (funcall test (funcall key (car rest)) item))
      (return rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if

(defun |member-if key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (car rest))
      (return rest))))

(defun |member-if key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if)
    (when (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if (predicate list &key key)
  (if key
      (|member-if key=other| predicate list key)
      (|member-if key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function member-if-not

(defun |member-if-not key=identity| (predicate list)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (car rest))
      (return rest))))

(defun |member-if-not key=other| (predicate list key)
  (with-proper-list-rests (rest list 'member-if-not)
    (unless (funcall predicate (funcall key (car rest)))
      (return rest))))

(defun member-if-not (predicate list &key key)
  (if key
      (|member-if-not key=other| predicate list key)
      (|member-if-not key=identity| predicate list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function acons

(defun acons (key datum alist)
  (cons (cons key datum) alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function pairlis

;;; The hyperspec says the consequences are undefined if the two
;;; lists do not have the same length.  We check for this situation
;;; and signal an error in that case. 

(defun pairlis (keys data &optional alist)
  (loop with result = alist
	with remaining-keys = keys
	with remaining-data = data
	until (or (atom remaining-keys) (atom remaining-data))
	do (push (cons (pop remaining-keys) (pop remaining-data)) result)
	finally (unless (and (null remaining-keys) (null remaining-data))
		  (cond ((and (atom remaining-keys) (not (null remaining-keys)))
			 (error 'must-be-proper-list
				:datum keys
				:name 'pairlis))
			((and (atom remaining-data) (not (null remaining-data)))
			 (error 'must-be-proper-list
				:datum data
				:name 'pairlis))
			(t
			 (error 'lists-must-have-the-same-length
				:list1 keys
				:list2 data
				:name 'pairlis))))
		(return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-alist

;;; The HyperSpec says that the argument is an alist, and 
;;; in those cases, whenever the type doesn't correspond, it
;;; is legitimate to signal an error.  However, for copy-alist,
;;; the HyperSpec also says that any object that is referred to
;;; directly or indirectly is still shared betwee the argument
;;; and the resulting list, which suggests that any element that
;;; is not a cons should just be included in the resulting
;;; list as well.  And that is what we are doing. 

;;; We use (loop for ... on ...) because it uses ATOM to test for
;;; the end of the list.  Then we check that the atom is really 
;;; null, and if not, signal a type error.

(defun copy-alist (alist)
  (loop for remaining on alist
	collect (if (consp (car remaining))
		    (cons (caar remaining) (cdar remaining))
		    (car remaining))
	finally (unless (null remaining)
		  (error 'must-be-proper-list
			 :datum alist
			 :name 'copy-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tailp

;;; We could use (loop for ... on ...) here for consistency. 

(defun tailp (object list)
  (loop for rest = list then (cdr rest)
	until (atom rest)
	when (eql object rest)
	  return t
	finally (return (eql object rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ldiff

(defun ldiff (list object)
  (if (or (eql list object) (atom list))
      nil
      (let* ((result (list (car list)))
             (current result)
             (remaining (cdr list)))
        (loop until (or (eql remaining object) (atom remaining))
              do (setf (cdr current) (list (car remaining)))
                 (setf current (cdr current))
                 (setf remaining (cdr remaining)))
        (if (eql remaining object)
            result
            (progn (setf (cdr current) remaining)
                   result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function union

(defun |union key=identity test=eql| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |union key=identity test=eq| (name list1 list2)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |union key=other test=eql| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |union key=other test=eq| (name list1 list2 key)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |union key=identity test=other| (name list1 list2 test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun |union key=other test=other| (name list1 list2 key test)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1 ) list2 test key)
	(push element1 result)))
    result))

(defun |union key=identity test-not=other| (name list1 list2 test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun |union key=other test-not=other| (name list1 list2 key test-not)
  (let ((result list2))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name element1 list2 test-not key)
	(push element1 result)))
    result))

(defun |union key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun |union key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp)))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table) element))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) element))
    (loop for element being the hash-values of table
	  collect element)))

(defun union (list1 list2
	      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'union))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=other test=eq hash| 'union list1 list2 key)
		       (|union key=other test=eq| 'union list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=other test=eql hash| 'union list1 list2 key)
		       (|union key=other test=eql| 'union list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=other test=equal hash| 'union list1 list2 key)
		       (|union key=other test=other| 'union list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=other test=equalp hash| 'union list1 list2 key)
		       (|union key=other test=other| 'union list1 list2 key #'equalp)))
		  (t
		   (|union key=other test=other| 'union list1 list2 key test)))
	    (if test-not-given
		(|union key=other test-not=other| 'union list1 list2 key test-not)
		(if use-hash
		    (|union key=other test=eql hash| 'union list1 list2 key)
		    (|union key=other test=eql| 'union list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=identity test=eq hash| 'union list1 list2)
		       (|union key=identity test=eq| 'union list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=identity test=eql hash| 'union list1 list2)
		       (|union key=identity test=eql| 'union list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=identity test=equal hash| 'union list1 list2)
		       (|union key=identity test=other| 'union list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=identity test=equalp hash| 'union list1 list2)
		       (|union key=identity test=other| 'union list1 list2 #'equalp)))
		  (t
		   (|union key=identity test=other| 'union list1 list2 test)))
	    (if test-not-given
		(|union key=identity test-not=other| 'union list1 list2 test-not)
		(if use-hash
		    (|union key=identity test=eql hash| 'union list1 list2)
		    (|union key=identity test=eql| 'union list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nunion

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nunion (list1 list2
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nunion))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=other test=eq hash| 'nunion list1 list2 key)
		       (|union key=other test=eq| 'nunion list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=other test=eql hash| 'nunion list1 list2 key)
		       (|union key=other test=eql| 'nunion list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=other test=equal hash| 'nunion list1 list2 key)
		       (|union key=other test=other| 'nunion list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=other test=equalp hash| 'nunion list1 list2 key)
		       (|union key=other test=other| 'nunion list1 list2 key #'equalp)))
		  (t
		   (|union key=other test=other| 'nunion list1 list2 key test)))
	    (if test-not-given
		(|union key=other test-not=other| 'nunion list1 list2 key test-not)
		(if use-hash
		    (|union key=other test=eql hash| 'nunion list1 list2 key)
		    (|union key=other test=eql| 'nunion list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|union key=identity test=eq hash| 'nunion list1 list2)
		       (|union key=identity test=eq| 'nunion list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|union key=identity test=eql hash| 'nunion list1 list2)
		       (|union key=identity test=eql| 'nunion list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|union key=identity test=equal hash| 'nunion list1 list2)
		       (|union key=identity test=other| 'nunion list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|union key=identity test=equalp hash| 'nunion list1 list2)
		       (|union key=identity test=other| 'nunion list1 list2 #'equalp)))
		  (t
		   (|union key=identity test=other| 'nunion list1 list2 test)))
	    (if test-not-given
		(|union key=identity test-not=other| 'nunion list1 list2 test-not)
		(if use-hash
		    (|union key=identity test=eql hash| 'nunion list1 list2)
		    (|union key=identity test=eql| 'nunion list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function intersection

(defun |intersection key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |intersection key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=identity| name element1 list2)
	(push element1 result)))
    result))

(defun |intersection key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eql key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |intersection key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=eq key=other| name (funcall key element1) list2 key)
	(push element1 result)))
    result))

(defun |intersection key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    result))

(defun |intersection key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    result))

(defun |intersection key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    result))

(defun |intersection key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (when (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    result))

(defun |intersection key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash element table)
	(push element result)))
    result))

(defun |intersection key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |intersection key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |intersection key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))
	 
(defun |intersection key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (when (gethash (funcall key element) table)
	(push element result)))
    result))

(defun intersection (list1 list2
		     &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'intersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=other test=eq hash| 'intersection list1 list2 key)
		       (|intersection key=other test=eq| 'intersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=other test=eql hash| 'intersection list1 list2 key)
		       (|intersection key=other test=eql| 'intersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=other test=equal hash| 'intersection list1 list2 key)
		       (|intersection key=other test=other| 'intersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=other test=equalp hash| 'intersection list1 list2 key)
		       (|intersection key=other test=other| 'intersection list1 list2 key #'equalp)))
		  (t
		   (|intersection key=other test=other| 'intersection list1 list2 key test)))
	    (if test-not-given
		(|intersection key=other test-not=other| 'intersection list1 list2 key test-not)
		(if use-hash
		    (|intersection key=other test=eql hash| 'intersection list1 list2 key)
		    (|intersection key=other test=eql| 'intersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=identity test=eq hash| 'intersection list1 list2)
		       (|intersection key=identity test=eq| 'intersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=identity test=eql hash| 'intersection list1 list2)
		       (|intersection key=identity test=eql| 'intersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=identity test=equal hash| 'intersection list1 list2)
		       (|intersection key=identity test=other| 'intersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=identity test=equalp hash| 'intersection list1 list2)
		       (|intersection key=identity test=other| 'intersection list1 list2 #'equalp)))
		  (t
		   (|intersection key=identity test=other| 'intersection list1 list2 test)))
	    (if test-not-given
		(|intersection key=identity test-not=other| 'intersection list1 list2 test-not)
		(if use-hash
		    (|intersection key=identity test=eql hash| 'intersection list1 list2)
		    (|intersection key=identity test=eql| 'intersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nintersection

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nintersection (list1 list2
		      &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nintersection))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=other test=eq hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=eq| 'nintersection list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=eql| 'nintersection list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=other test=equal hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=other| 'nintersection list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=other test=equalp hash| 'nintersection list1 list2 key)
		       (|intersection key=other test=other| 'nintersection list1 list2 key #'equalp)))
		  (t
		   (|intersection key=other test=other| 'nintersection list1 list2 key test)))
	    (if test-not-given
		(|intersection key=other test-not=other| 'nintersection list1 list2 key test-not)
		(if use-hash
		    (|intersection key=other test=eql hash| 'nintersection list1 list2 key)
		    (|intersection key=other test=eql| 'nintersection list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|intersection key=identity test=eq hash| 'nintersection list1 list2)
		       (|intersection key=identity test=eq| 'nintersection list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|intersection key=identity test=eql hash| 'nintersection list1 list2)
		       (|intersection key=identity test=eql| 'nintersection list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|intersection key=identity test=equal hash| 'nintersection list1 list2)
		       (|intersection key=identity test=other| 'nintersection list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|intersection key=identity test=equalp hash| 'nintersection list1 list2)
		       (|intersection key=identity test=other| 'nintersection list1 list2 #'equalp)))
		  (t
		   (|intersection key=identity test=other| 'nintersection list1 list2 test)))
	    (if test-not-given
		(|intersection key=identity test-not=other| 'nintersection list1 list2 test-not)
		(if use-hash
		    (|intersection key=identity test=eql hash| 'nintersection list1 list2)
		    (|intersection key=identity test=eql| 'nintersection list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-difference

(defun |set-difference key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    result))

(defun |set-difference key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    result))

(defun |set-difference key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun |set-difference key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    result))

(defun |set-difference key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=identity| name element list2  test)
	(push element result)))
    result))

(defun |set-difference key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=other key=other| name (funcall key element) list2 test key)
	(push element result)))
    result))

(defun |set-difference key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=identity| name element list2 test-not)
	(push element result)))
    result))

(defun |set-difference key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test-not=other key=other| name (funcall key element) list2 test-not key)
	(push element result)))
    result))

(defun |set-difference key=identity test=eq hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=eql hash| (name list1 list2)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=equal hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=identity test=equalp hash| (name list1 list2)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table)
	(push element result)))
    result))

(defun |set-difference key=other test=eq hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=eql hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=equal hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun |set-difference key=other test=equalp hash| (name list1 list2 key)
  (let ((table (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table)
	(push element result)))
    result))

(defun set-difference (list1 list2
		       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'set-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=other test=eq hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=eq| 'set-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=other test=eql hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=eql| 'set-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=other test=equal hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=other| 'set-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=other test=equalp hash| 'set-difference list1 list2 key)
		       (|set-difference key=other test=other| 'set-difference list1 list2 key #'equalp)))
		  (t
		   (|set-difference key=other test=other| 'set-difference list1 list2 key test)))
	    (if test-not-given
		(|set-difference key=other test-not=other| 'set-difference list1 list2 key test-not)
		(if use-hash
		    (|set-difference key=other test=eql hash| 'set-difference list1 list2 key)
		    (|set-difference key=other test=eql| 'set-difference list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=identity test=eq hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=eq| 'set-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=identity test=eql hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=eql| 'set-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=identity test=equal hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=other| 'set-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=identity test=equalp hash| 'set-difference list1 list2)
		       (|set-difference key=identity test=other| 'set-difference list1 list2 #'equalp)))
		  (t
		   (|set-difference key=identity test=other| 'set-difference list1 list2 test)))
	    (if test-not-given
		(|set-difference key=identity test-not=other| 'set-difference list1 list2 test-not)
		(if use-hash
		    (|set-difference key=identity test=eql hash| 'set-difference list1 list2)
		    (|set-difference key=identity test=eql| 'set-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-difference

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-difference (list1 list2
			&key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-difference))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=other test=eq hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=eq| 'nset-difference list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=eql| 'nset-difference list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=other test=equal hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=other test=equalp hash| 'nset-difference list1 list2 key)
		       (|set-difference key=other test=other| 'nset-difference list1 list2 key #'equalp)))
		  (t
		   (|set-difference key=other test=other| 'nset-difference list1 list2 key test)))
	    (if test-not-given
		(|set-difference key=other test-not=other| 'nset-difference list1 list2 key test-not)
		(if use-hash
		    (|set-difference key=other test=eql hash| 'nset-difference list1 list2 key)
		    (|set-difference key=other test=eql| 'nset-difference list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-difference key=identity test=eq hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=eq| 'nset-difference list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=eql| 'nset-difference list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-difference key=identity test=equal hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-difference key=identity test=equalp hash| 'nset-difference list1 list2)
		       (|set-difference key=identity test=other| 'nset-difference list1 list2 #'equalp)))
		  (t
		   (|set-difference key=identity test=other| 'nset-difference list1 list2 test)))
	    (if test-not-given
		(|set-difference key=identity test-not=other| 'nset-difference list1 list2 test-not)
		(if use-hash
		    (|set-difference key=identity test=eql hash| 'nset-difference list1 list2)
		    (|set-difference key=identity test=eql| 'nset-difference list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function adjoin

(defun |adjoin key=identity test=eq| (name item list)
  (if (|member test=eq key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=eql| (name item list)
  (if (|member test=eql key=identity| name item list)
      list
      (cons item list)))

(defun |adjoin key=identity test=other| (name item list test)
  (if (|member test=other key=identity| name item list test)
      list
      (cons item list)))

(defun |adjoin key=identity test-not=other| (name item list test-not)
  (if (|member test-not=other key=identity| name item list test-not)
      list
      (cons item list)))

(defun |adjoin key=other test=eq| (name item list key)
  (if (|member test=eq key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=eql| (name item list key)
  (if (|member test=eql key=other| name (funcall key item) list key)
      list
      (cons item list)))

(defun |adjoin key=other test=other| (name item list key test)
  (if (|member test=other key=other| name (funcall key item) list test key)
      list
      (cons item list)))

(defun |adjoin key=other test-not=other| (name item list key test-not)
  (if (|member test-not=other key=other| name (funcall key item) list test-not key)
      list
      (cons item list)))

(defun adjoin (item list 
	       &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'adjoin))
  (if key
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (|adjoin key=other test=eq| 'adjoin item list key)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (|adjoin key=other test=eql| 'adjoin item list key)
		  (|adjoin key=other test=other| 'adjoin item list key test)))
	  (if test-not-given
	      (|adjoin key=other test-not=other| 'adjoin item list key test-not)
	      (|adjoin key=other test=eql| 'adjoin item list key)))
      (if test-given
	  (if (or (eq test #'eq) (eq test 'eq))
	      (|adjoin key=identity test=eq| 'adjoin item list)
	      (if (or (eq test #'eql) (eq test 'eql))
		  (|adjoin key=identity test=eql| 'adjoin item list)
		  (|adjoin key=identity test=other| 'adjoin item list test)))
	  (if test-not-given
	      (|adjoin key=identity test-not=other| 'adjoin item list test-not)
	      (|adjoin key=identity test=eql| 'adjoin item list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function set-exclusive-or

(defun |set-exclusive-or key=identity test=eql| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=identity| name element list1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=eq| (name list1 list2)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=identity| name element list2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=identity| name  element list1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eql| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eql key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eql key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eq| (name list1 list2 key)
  (let ((result '()))
    (with-proper-list-elements (element list1 name)
      (unless (|member test=eq key=other| name (funcall key element) list2 key)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (|member test=eq key=other| name (funcall key element) list1 key)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=other| (name list1 list2 test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=identity| name element1 list2 test)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test=other key=identity| name element2 list1 test)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=other test=other| (name list1 list2 key test)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test=other key=other| name (funcall key element1) list2 test key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test=other key=other| name (funcall key element2) list1 test key)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=identity test-not=other| (name list1 list2 test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=identity| name element1 list2 test-not)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test-not=other key=identity| name element2 list1 test-not)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=other test-not=other| (name list1 list2 key test-not)
  (let ((result '()))
    (with-proper-list-elements (element1 list1 name)
      (unless (|member test-not=other key=other| name (funcall key element1) list2 test-not key)
	(push element1 result)))
    ;; we need to use a member with a test with reversed order or arguments
    (with-proper-list-elements (element2 list2 name)
      (unless (|member reversed test-not=other key=other| name (funcall key element2) list1 test-not key)
	(push element2 result)))
    result))

(defun |set-exclusive-or key=identity test=eq hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=eql hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=equal hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=identity test=equalp hash| (name list1 list2)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash element table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash element table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash element table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash element table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eq hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eq))
	(table2 (make-hash-table :test #'eq))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=eql hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'eql))
	(table2 (make-hash-table :test #'eql))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=equal hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equal))
	(table2 (make-hash-table :test #'equal))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun |set-exclusive-or key=other test=equalp hash| (name list1 list2 key)
  (let ((table1 (make-hash-table :test #'equalp))
	(table2 (make-hash-table :test #'equalp))
	(result '()))
    (with-proper-list-elements (element list1 name)
      (setf (gethash (funcall key element) table1) t))
    (with-proper-list-elements (element list2 name)
      (setf (gethash (funcall key element) table2) t))
    (with-proper-list-elements (element list1 name)
      (unless (gethash (funcall key element) table2)
	(push element result)))
    (with-proper-list-elements (element list2 name)
      (unless (gethash (funcall key element) table1)
	(push element result)))
    result))

(defun set-exclusive-or (list1 list2
			 &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'set-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=other test=eq hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eq| 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=other test=eql hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eql| 'set-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=other test=equal hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=other test=equalp hash| 'set-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (|set-exclusive-or key=other test=other| 'set-exclusive-or list1 list2 key test)))
	    (if test-not-given
		(|set-exclusive-or key=other test-not=other| 'set-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (|set-exclusive-or key=other test=eql hash| 'set-exclusive-or list1 list2 key)
		    (|set-exclusive-or key=other test=eql| 'set-exclusive-or list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eq hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eq| 'set-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eql hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eql| 'set-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equal hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equalp hash| 'set-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 #'equalp)))
		  (t
		   (|set-exclusive-or key=identity test=other| 'set-exclusive-or list1 list2 test)))
	    (if test-not-given
		(|set-exclusive-or key=identity test-not=other| 'set-exclusive-or list1 list2 test-not)
		(if use-hash
		    (|set-exclusive-or key=identity test=eql hash| 'set-exclusive-or list1 list2)
		    (|set-exclusive-or key=identity test=eql| 'set-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nset-exclusive-or

;;; We take advantage of the fact that the standard doesn't 
;;; require this function to have any side effects. 

(defun nset-exclusive-or (list1 list2
			  &key key (test nil test-given) (test-not nil test-not-given))
  (when (and test-given test-not-given)
    (error 'both-test-and-test-not-given :name 'nset-exclusive-or))
  (let ((use-hash (> (* (length list1) (length list2)) 1000)))
    (if key
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=other test=eq hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eq| 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=other test=equal hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=other test=equalp hash| 'nset-exclusive-or list1 list2 key)
		       (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key #'equalp)))
		  (t
		   (|set-exclusive-or key=other test=other| 'nset-exclusive-or list1 list2 key test)))
	    (if test-not-given
		(|set-exclusive-or key=other test-not=other| 'nset-exclusive-or list1 list2 key test-not)
		(if use-hash
		    (|set-exclusive-or key=other test=eql hash| 'nset-exclusive-or list1 list2 key)
		    (|set-exclusive-or key=other test=eql| 'nset-exclusive-or list1 list2 key))))
	(if test-given
	    (cond ((or (eq test #'eq) (eq test 'eq))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eq hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eq| 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'eql) (eq test 'eql))
		   (if use-hash
		       (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))
		  ((or (eq test #'equal) (eq test 'equal))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equal hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equal)))
		  ((or (eq test #'equalp) (eq test 'equalp))
		   (if use-hash
		       (|set-exclusive-or key=identity test=equalp hash| 'nset-exclusive-or list1 list2)
		       (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 #'equalp)))
		  (t
		   (|set-exclusive-or key=identity test=other| 'nset-exclusive-or list1 list2 test)))
	    (if test-not-given
		(|set-exclusive-or key=identity test-not=other| 'nset-exclusive-or list1 list2 test-not)
		(if use-hash
		    (|set-exclusive-or key=identity test=eql hash| 'nset-exclusive-or list1 list2)
		    (|set-exclusive-or key=identity test=eql| 'nset-exclusive-or list1 list2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function getf

(defun getf (plist indicator &optional default)
  (unless (typep plist 'list)
    (error 'must-be-property-list
	   :datum plist
	   :name 'getf))
  (loop for rest on plist by #'cddr
	do (unless (consp (cdr rest))
	     (error 'must-be-property-list
		    :datum plist
		    'getf))
	when (eq (car rest) indicator)
	  return (cadr rest)
	finally (unless (null rest)
		  (error 'must-be-property-list
		    :datum plist
		    'getf))
		(return default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setf expander for getf

(define-setf-expander getf (place indicator &optional default &environment env)
  (let ((indicator-var (gensym))
	(default-var (gensym))
	(value-var (gensym)))
    (multiple-value-bind (vars vals store-vars writer-form reader-form)
	(get-setf-expansion place env)
      (values (append vars (list indicator-var default-var))
	      (append vals (list indicator default))
	      (list value-var)
	      `(let ((,default-var ,default-var))
		 (declare (ignore ,default-var))
		 (loop for rest on ,reader-form by #'cddr
		       when (eq (car rest) ,indicator-var)
			 do (setf (cadr rest) ,value-var)
			    (return nil)
		       finally (let ((,(car store-vars)
				      (list* ,indicator-var ,value-var ,reader-form)))
				 ,writer-form))
		 ,value-var)
	      `(getf ,reader-form ,indicator-var ,default)))))
[
