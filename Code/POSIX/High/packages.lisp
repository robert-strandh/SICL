(cl:in-package #:common-lisp-user)

(defpackage #:sicl-posix-high
  (:use #:common-lisp)
  (:local-nicknames (#:low #:sicl-posix-low))
  (:shadow #:read
           #:write
           #:open
           #:close)
  (:export #:read
           #:write
           #:open
           #:close
           ;; Conditions.
           #:write-would-block
           #:invalid-file-descriptor
           #:destination-address-not-set
           #:write-exceeds-quota
           #:write-exceeds-max-size
           #:invalid-value
           #:low-level-io-error
           #:no-space-on-device
           #:file-sealed
           #:pipe-closed))
