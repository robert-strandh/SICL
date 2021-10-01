(cl:in-package #:sicl-posix-high)

(define-condition file-descriptor-mixin ()
  ((%file-descriptor :initarg :file-descriptor :reader file-descriptor)))

(define-condition file-descriptor-refers-to-directory
    (error file-descriptor-mixin)
  ())

(define-condition read-would-block
    (error file-descriptor-mixin)
  ())

(define-condition write-would-block
    (error file-descriptor-mixin)
  ())

(define-condition invalid-file-descriptor
    (error file-descriptor-mixin)
  ())

(define-condition destination-address-not-set
    (error file-descriptor-mixin)
  ())

(define-condition write-exceeds-quota
    (error file-descriptor-mixin)
  ())

(define-condition write-exceeds-max-size
    (error file-descriptor-mixin)
  ())

(define-condition invalid-value
    (error file-descriptor-mixin)
  ())

(define-condition low-level-io-error
    (error file-descriptor-mixin)
  ())

(define-condition no-space-on-device
    (error file-descriptor-mixin)
  ())

(define-condition file-sealed
    (error file-descriptor-mixin)
  ())

(define-condition pipe-closed
    (error file-descriptor-mixin)
  ())
