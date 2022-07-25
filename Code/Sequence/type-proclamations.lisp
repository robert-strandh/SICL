(cl:in-package #:sicl-sequence)

(declaim (ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                          (integer 0))
                count))

(declaim (ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                          (or null (integer 0)))
                position))

(declaim (ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function))
                          t)
                find))

(declaim (ftype (function (t sequence
                             &key
                             (:from-end t)
                             (:start (integer 0))
                             (:end (or (nil (integer 0))))
                             (:key function)
                             (:test function)
                             (:test-not function)
                             (:count (or null integer)))
                          sequence)
                remove))

(declaim (ftype (function (sequence) (integer 0))
                length))

(declaim (ftype (function (sequence) sequence)
                reverse nreverse))

(declaim (ftype (function (sequence function) sequence)
                sort stable-sort))

(declaim (ftype (function (sequence
                           (integer 0)
                           &optional (or null (integer 0)))
                          sequence)
                subseq))

(declaim (ftype (function (function sequence
                                    &key
                                    (:key function)
                                    (:start (integer 0))
                                    (:end (or null (integer 0)))
                                    (:key function)
                                    (:from-end t)
                                    (:initial-value t))
                          t)
                reduce))
