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

(proclaim '(ftype (function (sequence) (integer 0))
            length))
