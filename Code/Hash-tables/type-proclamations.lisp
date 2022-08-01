(cl:in-package #:sicl-hash-table)

(proclaim '(ftype (function
                   (&key
                    (:test (or symbol function))
                    (:size (integer 0))
                    (:rehash-size (or (integer 1) (float (1.0))))
                    (:rehash-threshold (real 0 1)))
                   hash-table)
            make-hash-table))
