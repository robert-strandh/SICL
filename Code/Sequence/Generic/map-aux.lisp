(cl:in-package #:sicl-sequence)

(macrolet
    ((define-map-over-iterators-n (n)
       (let ((name (enumerate-symbol 'map-over-iterators n))
             (readers (loop for index below n collect (enumerate-symbol 'reader index))))
         `(progn
            (declaim (inline ,name))
            (defun ,name (writer function ,@readers)
              (declare (function writer function ,@readers))
              (loop
                (funcall
                 writer
                 (funcall
                  function
                  ,@(loop for reader in readers collect `(funcall ,reader)))))))))
     (define-map-over-iterators (n)
       `(progn
          ,@(loop for index below n
                  collect `(define-map-over-iterators-n ,index))
          (defun map-over-iterators (writer function &rest readers)
            (case (length readers)
              ,@(loop for index below n
                      collect
                      `(,index (apply #',(enumerate-symbol 'map-over-iterators index)
                                      writer
                                      function
                                      readers)))
              (otherwise
               (loop
                 (funcall writer (apply function (mapcar #'funcall readers)))))))
          (define-compiler-macro map-over-iterators (&whole form writer function &rest readers)
            (case (length readers)
              ,@(loop for index below n
                      collect
                      `(,index `(,',(enumerate-symbol 'map-over-iterators index)
                                 ,,'writer
                                 ,,'function
                                 ,@,'readers)))
              (otherwise form))))))
  (define-map-over-iterators 7))

(macrolet
    ((define-map-nil-over-iterators-n (n)
       (let ((name (enumerate-symbol 'map-nil-over-iterators n))
             (readers (loop for index below n collect (enumerate-symbol 'reader index))))
         `(progn
            (declaim (inline ,name))
            (defun ,name (function ,@readers)
              (declare (function function ,@readers))
              (loop
                (funcall
                 (funcall
                  function
                  ,@(loop for reader in readers collect `(funcall ,reader)))))))))
     (define-map-nil-over-iterators (n)
       `(progn
          ,@(loop for index below n
                  collect `(define-map-nil-over-iterators-n ,index))
          (defun map-nil-over-iterators (function &rest readers)
            (case (length readers)
              ,@(loop for index below n
                      collect
                      `(,index (apply #',(enumerate-symbol 'map-nil-over-iterators index)
                                      function
                                      readers)))
              (otherwise
               (loop
                 (apply function (mapcar #'funcall readers))))))
          (define-compiler-macro map-nil-over-iterators (&whole form function &rest readers)
            (case (length readers)
              ,@(loop for index below n
                      collect
                      `(,index `(,',(enumerate-symbol 'map-nil-over-iterators index)
                                 ,,'function
                                 ,@,'readers)))
              (otherwise form))))))
  (define-map-nil-over-iterators 8))
