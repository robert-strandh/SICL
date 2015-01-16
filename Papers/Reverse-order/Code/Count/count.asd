;;; -*- Mode: Lisp -*-
;;;  (c) copyright 2015 by
;;;           Irene Durand (idurand@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ASDF system definition for Count.

(cl:in-package #:asdf-user)

(defsystem :count
  :description "Reverse Count experimentations"
  :name "count"
  :version "1.0"
  :author "Irene Durand <idurand@labri.fr>"
  :licence "General Public Licence"
  :depends-on ()
  :serial t
  :components
  (
   (:file "general") 
   (:file "0-count")
   (:file "1-count")
   (:file "2-count")
   (:file "3-count")
   (:file "4-count")
   (:file "5-count")
   (:file "6-count")
   (:file "7-count")
   (:file "9-count"))
  :serial t)

;; (pushnew :count *features*)
