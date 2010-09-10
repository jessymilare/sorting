;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :sorting-system
  (:use :cl :asdf))

(in-package :sorting-system)

(defsystem sorting-benchmark
  :name "Sorting Benchmark"
  :version "0.0.3"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Benchmark system that tests the performance of sorting
algorithms in various situations."
  :depends-on (trivial-garbage alexandria sorting)
  :components ((:module "bench"
                :components ((:file "package")
                             (:file "setup" :depends-on ("package"))
                             (:file "minibench" :depends-on ("setup"))
                             (:file "definitions" :depends-on ("minibench"))))))
