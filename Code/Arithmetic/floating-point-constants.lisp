(cl:in-package #:sicl-arithmetic)

(defconstant most-positive-single-float
  (float-features:bits-single-float #x7f7fffff))

(defconstant least-positive-single-float
  (float-features:bits-single-float #x00000001))

(defconstant least-positive-normalized-single-float
  (float-features:bits-single-float #x00800000))

(defconstant most-negative-single-float
  (float-features:bits-single-float #xff7fffff))

(defconstant least-negative-single-float
  (float-features:bits-single-float #x80000001))

(defconstant least-negative-normalized-single-float
  (float-features:bits-single-float #x80800000))

(defconstant most-positive-double-float
  (float-features:bits-double-float #x7fefffffffffffff))

(defconstant least-positive-double-float
  (float-features:bits-double-float #x0000000000000001))

(defconstant least-positive-normalized-double-float
  (float-features:bits-double-float #x0010000000000000))

(defconstant most-negative-double-float
  (float-features:bits-double-float #xffefffffffffffff))

(defconstant least-negative-double-float
  (float-features:bits-double-float #x8000000000000001))

(defconstant least-negative-normalized-double-float
  (float-features:bits-double-float #x8010000000000000))

(defconstant most-positive-short-float
  most-positive-single-float)

(defconstant least-positive-short-float
  least-positive-single-float)

(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)

(defconstant most-negative-short-float
  most-negative-single-float)

(defconstant least-negative-short-float
  least-negative-single-float)

(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)

(defconstant most-positive-long-float
  most-positive-double-float)

(defconstant least-positive-long-float
  least-positive-double-float)

(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)

(defconstant most-negative-long-float
  most-negative-double-float)

(defconstant least-negative-long-float
  least-negative-double-float)

(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)
