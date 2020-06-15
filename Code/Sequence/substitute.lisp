(cl:in-package #:sicl-sequence)

(progn
  (replicate-for-each-vector-class #1=#:vector-class
    (defmethod substitute (newitem olditem (sequence #1#)
                           &key from-end test test-not (start 0) end count key)
      #2=
      (let ((count (canonicalize-count count)))
        (if (zerop count)
            sequence
            (let ((pos (position olditem sequence
                                 :key key :start start :end end
                                 :test test :test-not test-not :from-end from-end)))
              (if (not pos)
                  sequence
                  (let ((copy (copy-seq sequence)))
                    (setf (elt copy pos) newitem)
                    (if (not from-end)
                        (nsubstitute newitem olditem copy
                                     :key key :start (1+ pos) :end end :count (1- count)
                                     :test test :test-not test-not :from-end nil)
                        (nsubstitute newitem olditem copy
                                     :key key :start start :end pos :count (1- count)
                                     :test test :test-not test-not :from-end t)))))))))

  (defmethod substitute (newitem olditem (sequence list)
                         &key from-end test test-not (start 0) end count key)
    #2#))

(seal-domain #'substitute '(t t vector))
(seal-domain #'substitute '(t t list))
