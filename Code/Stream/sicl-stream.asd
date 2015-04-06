(cl:in-package #:asdf-user)

(defsystem :sicl-stream
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "stream")
   (:file "standard-stream")
   (:file "unix-byte-stream")
   (:file "with-open-stream-defmacro")))
