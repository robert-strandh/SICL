(cl:in-package #:sicl-mir-to-lir)

(defvar *used-labels*)
(defvar *label-count*)
(defvar *raw-integer-count*)
(defvar *raw-integers*)

(defun new-label (object)
  (let* ((label-number (incf *label-count*))
         (label-text (format nil "L~d" label-number)))
    (setf (gethash object *used-labels*) label-text)))

(defun print-lir (lir)
  (let ((*used-labels* (make-hash-table :test 'eq))
        (*label-count* 0)
        (*gensym-counter* 0)
        (*raw-integer-count* 0)
        (*raw-integers* (make-hash-table :test #'eq)))
    (write-instruction-chain lir)
    lir))

(defgeneric location-name (location)
  (:method ((location cleavir-ir:register-location))
    (cleavir-ir:name location))
  (:method ((immediate cleavir-ir:immediate-input))
    (prin1-to-string (cleavir-ir:value immediate)))
  (:method ((lexical cleavir-ir:lexical-location))
    (cleavir-ir:name lexical))
  (:method ((lexical cleavir-ir:raw-integer))
    (let ((name (gethash lexical *raw-integers*)))
      (if (null name)
          (setf (gethash lexical *raw-integers*)
                (format nil "temp~d" (incf *raw-integer-count*)))
          name))))

(defun write-instruction-chain (instruction)
  (cond
    ((gethash instruction *used-labels*)
     (format t "~&goto ~a"
             (gethash instruction *used-labels*)))
    (t
     (when (> (length (cleavir-ir:successors instruction)) 1)
       (format t "~&~a:" (new-label instruction)))
     ;; write-lir-instruction might write out the successors itself, and it
     ;; will return T if it has.
     (unless (write-lir-instruction instruction)
       (mapc #'write-instruction-chain (cleavir-ir:successors instruction))))))

(defgeneric write-lir-instruction (instruction)
  (:method ((instruction cleavir-ir:instruction))
    #-swank (print instruction)
    #+swank (swank::present-repl-results (list instruction))
    nil)
  (:method ((top-level cleavir-ir:top-level-enter-instruction))
    (format t "; dynamic environment = ~a"
            (location-name (cleavir-ir:dynamic-environment-location
                            top-level))))
  (:method ((assignment cleavir-ir:assignment-instruction))
    (format t "~&mov ~a, ~a"
            (location-name (first (cleavir-ir:outputs assignment)))
            (location-name (first (cleavir-ir:inputs assignment)))))
  (:method ((memset cleavir-ir:memset1-instruction))
    (format t "~&mov [~a], ~a"
            (location-name (first  (cleavir-ir:inputs memset)))
            (location-name (second (cleavir-ir:inputs memset)))))
  (:method ((memref cleavir-ir:memref1-instruction))
    (format t "~&mov ~a, [~a]"
            (location-name (first (cleavir-ir:outputs memref)))
            (location-name (first (cleavir-ir:inputs memref)))))
  (:method ((sub cleavir-ir:unsigned-sub-instruction))
    (assert (eq (first (cleavir-ir:inputs sub))
                (first (cleavir-ir:outputs sub))))
    (format t "~&sub ~a, ~a"
            (location-name (first  (cleavir-ir:inputs sub)))
            (location-name (second (cleavir-ir:inputs sub)))))
  (:method ((add cleavir-ir:unsigned-add-instruction))
    (assert (eq (first (cleavir-ir:inputs add))
                (first (cleavir-ir:outputs add))))
    (format t "~&add ~a, ~a"
            (location-name (first  (cleavir-ir:inputs add)))
            (location-name (second (cleavir-ir:inputs add)))))
  (:method ((funcall cleavir-ir:funcall-instruction))
    (format t "~&FUNCALL ~a"
            (location-name (first (cleavir-ir:inputs funcall)))))
  (:method ((nop cleavir-ir:nop-instruction))
    (format t "~&nop"))
  (:method ((ret cleavir-ir:return-instruction))
    (format t "~&ret"))
  (:method ((shl cleavir-ir:shift-left-instruction))
    (if (eq (first (cleavir-ir:outputs shl))
            (first (cleavir-ir:inputs shl)))
        (format t "~&shl ~a, ~a"
                (location-name (first  (cleavir-ir:inputs shl)))
                (location-name (second (cleavir-ir:inputs shl))))
        (format t "~&IMPOSSIBLE SHIFT-LEFT: ~a <- ~a << ~a"
                (location-name (first (cleavir-ir:outputs shl)))
                (location-name (first (cleavir-ir:inputs shl)))
                (location-name (second (cleavir-ir:inputs shl))))))
  (:method ((less cleavir-ir:unsigned-less-instruction))
    (format t "~&cmp ~a, ~a"
            (location-name (first (cleavir-ir:inputs less)))
            (location-name (second (cleavir-ir:inputs less))))
    (let ((then (gensym "THEN"))
          (else (gensym "ELSE")))
      (format t "~&jb ~a~&jmp ~a" then else)
      (format t "~&~a:" then)
      (write-instruction-chain (cleavir-ir:first-successor less))
      (format t "~&~a:" else)
      (write-instruction-chain (second (cleavir-ir:successors less))))
    t))
