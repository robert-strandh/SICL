(cl:in-package #:sicl-elf)

(defparameter *file-class*
  '((1 . :32-bit) (2 . :64-bit)))

(defparameter *data-encoding*
  '((1 . :little-endian) (2 . :big-endian)))

(defparameter *file-version*
  '((1 . :original-version)))

(defparameter *os/abi-identification*
  '((#x00 . :system-v)
    (#x01 . :hp-ux)
    (#x02 . :netbsd)
    (#x03 . :linux)
    (#x04 . :gnu-hurd)
    ;; No #x05 apparently.
    (#x06 . :solaris)
    (#x07 . :aix)
    (#x08 . :irix)
    (#x09 . :freebsd)
    (#x0a . :tru64)
    (#x0b . :novell-modesto)
    (#x0c . :openbsd)
    (#x0d . :openvms)
    (#x0e . :nonstop-kernel)
    (#x0f . :aros)
    (#x10 . :fenix-os)
    (#x11 . :cloudabi)
    (#x12 . :stratus-technologies-openvos)))

(defclass elf ()
  ((%file-class
    :initarg :file-class
    :accessor file-class)
   (%data-encoding
    :initarg :data-encoding
    :accessor data-encoding)
   (%file-version
    :initarg :file-version
    :accessor file-version)
   (%os/abi-identification
    :initarg :os/abi-identification
    :accessor os/abi-identification)
   (%abi-version
    :initarg :abi-version
    :accessor abi-version))
  
   
