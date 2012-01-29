(in-package #:sicl-system)

;;; The value cells of these symbols will contain the corresponding
;;; class objects.

(defunbound *class-fixnum*)

(defunbound *class-cons*)

(defunbound *class-character*)

(defunbound *class-vector*)

(defunbound *class-string*)

(defunbound *class-symbol*)

(defunbound *class-package*)

(defunbound *class-code*)

(defunbound *class-function*)

(defunbound *class-dynamic-frame*)

(defunbound *class-unwind-protect*)

;;; The value cells of these symbols will contain vectors containing
;;; machine instructions for various linkage purposes.

(defunbound *linkage-error*)

(defunbound *linkage-function*)

(defunbound *linkage-symbol*)

