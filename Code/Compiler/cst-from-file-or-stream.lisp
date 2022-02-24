(cl:in-package #:sicl-compiler)

(defun read-cst (input-stream eof-marker)
  (eclector.concrete-syntax-tree:read input-stream nil eof-marker))

(defun cst-from-stream (input-stream)
  (let* ((csts (loop with eof-marker = input-stream
                     for cst = (read-cst input-stream eof-marker)
                     until (eq cst eof-marker)
                     collect cst)))
    (cst:cons (cst:cst-from-expression 'progn)
              (cst:cstify csts))))

(defun cst-from-file (file-name &key (source-tracking-p t))
  (if source-tracking-p
      (sicl-source-tracking:with-source-tracking-stream-from-file
          (input-stream file-name)
        (cst-from-stream input-stream))
      (with-open-file (input-stream file-name :direction :input)
        (cst-from-stream input-stream))))
