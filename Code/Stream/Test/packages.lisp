(cl:in-package #:common-lisp-user)

(defpackage #:sicl-stream
  (:use #:common-lisp)
  (:shadow #:finish-output
           #:force-output
           #:clear-output
           #:stream
           #:streamp
           #:input-stream-p
           #:output-stream-p
           #:interactive-stream-p
           #:close
           #:write-byte
           #:stream-element-type
           #:with-open-stream
           #:with-open-file)
  (:export
   #:finish-output
   #:force-output
   #:clear-output
   #:stream
   #:stream-p
   #:input-stream-p
   #:output-stream-p
   #:interactive-stream-p
   #:close
   #:write-byte
   #:stream-element-type
   #:with-open-stream
   #:with-open-file))
