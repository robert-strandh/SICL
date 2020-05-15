(cl:in-package #:sicl-sequence)

(progn
  (replicate-for-each-vector-class #1=#:vector-class
    (defmethod substitute-if (newitem predicate (sequence #1#)
                              &key from-end (start 0) end count key)
      #2=
      (let ((count (canonicalize-count count)))
        (if (zerop count)
            sequence
            (let ((pos (position-if predicate sequence
                                    :key key :start start :end end
                                    :from-end from-end)))
              (if (not pos)
                  sequence
                  (if (not from-end)
                      (nsubstitute-if newitem predicate (copy-seq sequence)
                                      :key key :start pos :end end
                                      :count count :from-end nil)
                      (nsubstitute-if newitem predicate (copy-seq sequence)
                                      :key key :start start :end (1+ pos)
                                      :count count :from-end t))))))))

  (defmethod substitute-if (newitem predicate (sequence list)
                            &key from-end (start 0) end count key)
    #2#))
