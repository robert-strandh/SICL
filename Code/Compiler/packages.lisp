(cl:in-package #:common-lisp-user)

(defpackage #:sicl-compiler
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:shadow #:undefined-function)
  (:export #:debug-information
           #:code-object
           #:instructions
           #:literals
           #:call-sites
           #:ensure-literal
           #:establish-call-site
           #:function-names
           #:hir
           #:tie-code-object
           #:ast-from-file
           #:ast-from-stream
           #:undefined-function
           #:undefined-variable
           #:undefined-block
           #:undefined-tagbody-tag
           #:call-site
           #:instruction
           #:name
           #:arguments
           #:offset
           #:instructions))
