;;; -*- Mode: Lisp -*-
;;;  (c) copyright 2016 by
;;;           Irene Durand (irene.durand@u-bordeaux.fr)

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

;;; ASDF system definition for Find.

(cl:in-package #:asdf-user)

(defsystem :find
  :description "Find experimentations"
  :name "find"
  :version "1.0"
  :author "Irene Durand <irene.durand@u-bordeaux.fr>"
  :licence "General Public Licence"
  :depends-on (:paper-sequence)
  :serial t
  :components
  (
   (:file "package") 
   (:file "general") 
;;   (:file "find-vector")
   (:file "find")
;;   (:file "find-list")
   (:file "tests")
   )
  :serial t)

;; (pushnew :find *features*)
