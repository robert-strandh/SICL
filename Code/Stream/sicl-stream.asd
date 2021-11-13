(cl:in-package #:asdf-user)

(defsystem :sicl-stream
  :depends-on (#:cyclosis)
  :serial t
  :components
  ((:file "packages")
   (:file "overrides")
   (:file "unix-byte-stream")
   (:file "character-to-binary-output-stream")
   (:file "binary-to-character-input-stream")))
