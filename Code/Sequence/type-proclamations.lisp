(cl:in-package #:sicl-sequence)

(proclaim '(ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function)))
            count))
