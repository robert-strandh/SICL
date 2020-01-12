(cl:in-package #:sicl-utilities)

(defmacro with-collectors (collectors &body body)
  "Within the lexical scope of BODY, bind the specified collectors.  Each
entry in COLLECTORS must be a list of two valid function names - RESULT and
GATHER.  Each such pair of function names is bound to local functions, with
the following behavior:

The function named GATHER takes a single argument, stores this argument in
some unspecified way, and returns this argument.

The function named RESULT takes either zero or one object.  It returns the
list of all objects that have been stored by calls to the function named
GATHER.  If supplied, the optional argument is used as the tail of the list
being returned.  Calling the function named RESULT more than once results
in undefined behavior.

Examples:

  (with-collectors ((odds collect-odd)
                    (evens collect-even))
    (loop for n below 8 do
      (if (oddp n)
          (collect-odd n)
          (collect-even n)))
    (values (odds) (evens)))

 => (1 3 5 7)
 => (0 2 4 6)

 (with-collectors ((numbers collect-number))
   (collect-number 2)
   (collect-number 3)
   (numbers '(4 5 6))) ; Here, we explicitly supply a tail.

 => (2 3 4 5 6)
"
  (if (null collectors)
      `(progn ,@body)
      `(with-collector ,(first collectors)
         (with-collectors ,(rest collectors)
           ,@body))))

(defmacro with-collector ((result gather) &body body)
  (let ((head (gensym))
        (tail (gensym)))
    `(let* ((,head (cons nil nil))
            (,tail ,head))
       (declare (cons ,head ,tail) (dynamic-extent ,head))
       (flet ((,result (&optional (tail nil tail-supplied-p))
                (when tail-supplied-p
                  (setf (cdr ,tail) tail))
                (cdr ,head))
              (,gather (object)
                (let ((cons (cons object nil)))
                  (setf (cdr ,tail) cons)
                  (setf ,tail cons)
                  object)))
         (declare (inline ,result ,gather))
         ,@body))))
