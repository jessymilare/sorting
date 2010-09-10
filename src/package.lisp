;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :sorting
  (:use :cl :alexandria)
  (:export
   #:insertion-sort #:bubble-sort #:selection-sort
   #:quicksort-simple #:quicksort-with-mean #:quicksort-stable
   #:merge-sort #:heapsort))

(in-package :sorting)

(define-constant +declare-for-sort+
    '(declare (optimize speed (safety 0) (debug 0) (space 0))
      (type simple-vector vector)
      (type function predicate))
  :test 'equal)
