(defpackage #:sicl-cons-high
    (:use #:cl)
  (:shadow . #1=(#:caar #:cadr #:cdar #:cddr
                 #:caaar #:caadr #:cadar #:caddr
                 #:cdaar #:cdadr #:cddar #:cdddr
                 #:caaaar #:caaadr #:caadar #:caaddr
                 #:cadaar #:cadadr #:caddar #:cadddr
                 #:cdaaar #:cdaadr #:cdadar #:cdaddr
                 #:cddaar #:cddadr #:cdddar #:cddddr
                 #:first #:second #:third #:fourth #:fifth
                 #:sixth #:seventh #:eighth #:ninth #:tenth
                 #:list #:list* #:last
                 #:copy-list #:list-length #:make-list
                 #:nth #:nthcdr))
  (:export . #1#))

(defpackage #:sicl-cons-high-test
    (:shadowing-import-from #:sicl-cons-high .
                            #.(package-shadowing-symbols '#:sicl-cons-high))
    (:use #:sicl-cons-high #:cl #:lisp-unit))

