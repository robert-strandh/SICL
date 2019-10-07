(cl:in-package #:sicl-cons)

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
