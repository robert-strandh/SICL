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
                             (:key function))
                          (or null (integer 0)))
                position-if position-if-not))

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

(declaim (ftype (function (function
                           sequence
                           &key
                           (:from-end t)
                           (:start (integer 0))
                           (:end (or (nil (integer 0))))
                           (:key function))
                          t)
                find-if find-if-not))

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
                remove delete))

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
                                    (:from-end t)
                                    (:initial-value t))
                          t)
                reduce))

(declaim (ftype (function (sequence (integer 0)) t)
                elt))

(declaim (ftype (function (sequence
                           &key
                           (:key function)
                           (:start (integer 0))
                           (:end (or null (integer 0)))
                           (:from-end t)
                           (:test function)
                           (:test-not function))
                          sequence)
                remove-duplicates
                delete-duplicates))

(declaim (ftype (function (sequence
                           sequence
                           &key
                           (:start1 (integer 0))
                           (:end1 (or null (integer 0)))
                           (:start2 (integer 0))
                           (:end2 (or null (integer 0))))
                          sequence)
                replace))

(declaim (ftype (function (sequence
                           sequence
                           &key
                           (:key function)
                           (:from-end t)
                           (:test function)
                           (:test-not function)
                           (:start1 (integer 0))
                           (:end1 (or null (integer 0)))
                           (:start2 (integer 0))
                           (:end2 (or null (integer 0))))
                          (or null (integer 0)))
                mismatch))

(declaim (ftype (function (sequence) sequence)
                copy-seq))
