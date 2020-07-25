(cl:in-package #:sicl-hir-evaluator-test)

(test `(if (foo 10) 11 12))

(test `(if (bar 10) 11 12))

(test `(let ((x 10))
         (+ x 20)))

(test `(let ((x 10))
         (+ x x (let ((x 11)) (+ x 20)))))

(test `(let* ((x 10)
              (y 20)
              (x (+ x 19)))
         (+ x y)))

(test `(let ((*x* 10))
         (+ *x* (bar *x*) *x* 20)))

(test `(let ((x 10))
         (block hello
           (return-from hello (+ x 20))
           50)))

(test `(let ((x 10))
         (block hello
           (let ((y 20))
             (return-from hello (+ x y)))
           50)))

(test `(let ((x 10))
         (tagbody
            (setq x (+ x 5))
            (go hello)
            (setq x (+ x 7))
          hello
            (setq x (+ x 6))
            (go out)
          hi
            (setq x (+ x 1))
          out)
         x))

(test `(let ((x 10))
         (tagbody
            (let ((y 5))
              (setq x (+ x y))
              (go hello))
            (setq x (+ x 7))
          hello
            (setq x (+ x 6))
            (go out)
          hi
            (setq x (+ x 1))
          out)
         x))

(test `(flet ((stuff (x) (+ x *x*)))
         (let ((*x* 10))
           (stuff 20))))

(test `(cleavir-primop:car (cons 1 2)))

(test `(cleavir-primop:cdr (cons 1 2)))

(test `(let ((cons (cons 1 nil)))
         (cleavir-primop:rplaca cons 2)
         cons))

(test `(let ((cons (cons nil 1)))
         (cleavir-primop:rplacd cons 2)
         cons))

(test `(if (null '()) 1 2))

(test `(if (cleavir-primop:eq (foo 10) 234) 1 2))

(test `(labels ((is-even (n)
                  (if (zerop n)
                      t
                      (is-odd (1- n))))
                (is-odd (n)
                  (if (zerop n)
                      nil
                      (is-even (1- n)))))
         (list (is-even 33) (is-odd 33))))

(test `(multiple-value-prog1 (bar 10) (foo 30)))

(test `(let ((x (multiple-value-prog1 (bar 10) (foo 30))))
         x))

(test `(flet ((baz (x)
                (values x x x)))
         (multiple-value-prog1 (baz 42)
           (foo 30))))

(test `(cleavir-primop:multiple-value-call #'values (bar 3)))
