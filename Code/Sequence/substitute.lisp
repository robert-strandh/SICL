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
                  (if (not from-end)
                      (nsubstitute newitem olditem (copy-seq sequence)
                                   :key key :start pos :end end :count count
                                   :test test :test-not test-not :from-end nil)
                      (nsubstitute newitem olditem (copy-seq sequence)
                                   :key key :start start :end (1+ pos) :count count
                                   :test test :test-not test-not :from-end t))))))))

  (defmethod substitute (newitem olditem (sequence list)
                         &key from-end test test-not (start 0) end count key)
    #2#))

(seal-domain #'substitute '(t t vector))
(seal-domain #'substitute '(t t list))
