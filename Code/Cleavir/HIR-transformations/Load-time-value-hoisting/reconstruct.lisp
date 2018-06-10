(cl:in-package #:cleavir-load-time-value-hoisting)

;;; Reconstruction is the process of generating, for an already scanned HIR
;;; flowchart, a thunk that returns a vector of constructed values, and a
;;; hash table, mapping each datum to the corresponding index in that
;;; vector.
