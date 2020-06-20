(cl:in-package #:sicl-package)

(defgeneric name (package))

(defgeneric (setf name) (new-name package))

(defgeneric nicknames (package))

(defgeneric (setf nicknames) (new-nicknames package))

(defgeneric local-nicknames (package))

(defgeneric (setf local-nicknames) (new-local-nicknames package))

(defgeneric use-list (package))

(defgeneric (setf use-list) (new-use-list package))

(defgeneric used-by-list (package))

(defgeneric (setf used-by-list) (new-used-by-list package))

(defgeneric external-symbols (package))

(defgeneric internal-symbols (package))

(defgeneric shadowing-symbols (package))

(defgeneric (setf shadowing-symbols) (new-shadowing-symbols package))
