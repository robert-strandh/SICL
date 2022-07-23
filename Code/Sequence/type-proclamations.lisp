(cl:in-package #:sicl-sequence)

(proclaim '(ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                   (integer 0))
            count))

(proclaim '(ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                   (or null (integer 0)))
            position))

(proclaim '(ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                   t)
            find))

(proclaim '(ftype (function (sequence) (integer 0))
            length))

(proclaim '(ftype (or
                   (function (list) list)
                   (function (vector) vector))
            reverse nreverse))

(proclaim '(ftype (or
                   (function (list function) list)
                   (function (vector function) vector))
            sort stable-sort))

(proclaim '(ftype (or
                   (function (list
                              (integer 0)
                              &optional (or null (integer 0)))
                    list)
                   (function (vector
                              (integer 0)
                              &optional (or null (integer 0)))
                    vector))
            subseq))
