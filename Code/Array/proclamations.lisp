(in-package #:sicl-array)

(declaim (ftype (function ((or list (integer 0))
                           &key
                           (:element-type t)
                           (:initial-element t)
                           (:initial-contents t)
                           (:adjustable t)
                           (:fill-pointer (or null (eql t) (integer 0)))
                           (:displaced-to (or array null))
                           (:displaced-index-offset (integer 0)))
                          array)
                make-array))

(declaim (ftype (function (t) (member nil t))
                arrayp))
