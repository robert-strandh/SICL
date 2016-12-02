;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 25 18:48:59 2002
;;;; Contains: Tests of LOOP

(in-package :cl-test)

;;; Simple loops
(deftest sloop.1
  (loop (return 'a))
  a)

(deftest sloop.2
  (loop (return (values))))

(deftest sloop.3
  (loop (return (values 'a 'b 'c 'd)))
  a b c d)

(deftest sloop.4
  (block nil
    (loop (return 'a))
    'b)
  b)

(deftest sloop.5
  (let ((i 0) (x nil))
    (loop
     (when (>= i 4) (return x))
     (incf i)
     (push 'a x)))
  (a a a a))

(deftest sloop.6
  (let ((i 0) (x nil))
    (block foo
      (tagbody
       (loop
        (when (>= i 4) (go a))
        (incf i)
        (push 'a x))
       a
       (return-from foo x))))
  (a a a a))

(deftest sloop.7
  (catch 'foo
    (let ((i 0) (x nil))
    (loop
     (when (>= i 4) (throw 'foo x))
     (incf i)
     (push 'a x))))
  (a a a a))

;;; Loop errors

(def-macro-test loop.error.1 (loop))

(deftest loop-finish.error.1
  (block done
    (loop
     for i from 1 to 10
     do (macrolet
            ((%m (&environment env)
                 (let ((mfn (macro-function 'loop-finish env)))
                   (cond
                    ((not mfn) '(return-from done :fail1))
                    ((not (eval `(signals-error (funcall ,mfn)
                                                program-error)))
                     '(return-from done :fail2))
                    ((not (eval `(signals-error (funcall ,mfn
                                                         '(loop-finish))
                                                program-error)))
                     '(return-from done :fail3))

                    ((not (eval `(signals-error (funcall ,mfn
                                                         '(loop-finish)
                                                         nil nil)
                                                program-error)))
                     '(return-from done :fail4))
                    (t '(return-from done :good))))))
          (%m))))
  :good)
