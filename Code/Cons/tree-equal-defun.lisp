(cl:in-package #:sicl-cons)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function tree-equal 

(defun |tree-equal test=eq| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eq tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=eq| (car tree1) (car tree2))
           (|tree-equal test=eq| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test=eql| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (eql tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=eql| (car tree1) (car tree2))
           (|tree-equal test=eql| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test-not=eq| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eq tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=eq| (car tree1) (car tree2))
           (|tree-equal test-not=eq| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test-not=eql| (tree1 tree2)
  (or (and (atom tree1)
           (atom tree2)
           (not (eql tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=eql| (car tree1) (car tree2))
           (|tree-equal test-not=eql| (cdr tree1) (cdr tree2)))))

(defun |tree-equal test=other| (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (funcall test tree1 tree2))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test=other| (car tree1) (car tree2) test)
           (|tree-equal test=other| (cdr tree1) (cdr tree2) test))))

(defun |tree-equal test-not=other| (tree1 tree2 test)
  (or (and (atom tree1)
           (atom tree2)
           (not (funcall test tree1 tree2)))
      (and (consp tree1)
           (consp tree2)
           (|tree-equal test-not=other| (car tree1) (car tree2) test)
           (|tree-equal test-not=other| (cdr tree1) (cdr tree2) test))))

(defun tree-equal (tree1 tree2
                   &key
                   (test nil testp)
                   (test-not nil test-not-p))
  (when (and testp test-not-p)
    (error 'both-test-and-test-not-given
           :name 'tree-equal))
  (if testp
      (if (eq test #'eq)
          (|tree-equal test=eq| tree1 tree2)
          (if (eq test #'eql)
              (|tree-equal test=eql| tree1 tree2)
              (|tree-equal test=other| tree1 tree2 test)))
      (if test-not-p 
          (if (eq test-not #'eq)
              (|tree-equal test-not=eq| tree1 tree2)
              (if (eq test-not #'eql)
                  (|tree-equal test-not=eql| tree1 tree2)
                  (|tree-equal test-not=other| tree1 tree2 test-not)))
          (|tree-equal test=eql| tree1 tree2))))
