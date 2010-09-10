;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :sorting-system
  (:use :cl :asdf))

(in-package :sorting-system)

(defsystem sorting
  :name "Sorting"
  :version "0.0.3"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Various sorting algorithms."
  :depends-on (alexandria)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "simple" :depends-on ("package"))
                             (:file "quicksort" :depends-on ("simple"))
                             (:file "other-sorts" :depends-on ("simple"))))))
