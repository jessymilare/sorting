;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :sorting)

(defun insertion-sort (vector predicate)
  #.+declare-for-sort+
  (loop for i from 1 below (length vector)
     for value = (svref vector i) do
       (loop for j from (1- i) downto 0
          for vector-j = (svref vector j) do
            (if (funcall predicate value vector-j)
                (setf (svref vector (1+ j)) vector-j)
                (progn
                  (setf (svref vector (1+ j)) value)
                  (return)))
            finally (setf (svref vector 0) value)))
  vector)

(defun bubble-sort (vector predicate)
  #.+declare-for-sort+
  (loop for i from (1- (length vector)) above 0 do
     (loop with changed = nil
        for j from 1 to i
        with prev = (svref vector 0)
        for cur = (svref vector j) do
          (if (funcall predicate cur prev)
              (setf (svref vector (1- j)) cur
                    (svref vector j) prev
                    ;; prev doesn't change
                    changed t)
              (setf prev cur))
        finally (unless changed
                  (return-from bubble-sort vector))))
  vector)

(defun selection-sort (vector predicate)
  #.+declare-for-sort+
  (loop with length = (length vector)
     for i below length
     for vector-i = (svref vector i) do
       (loop for j from (1+ i) below length
          for vector-j = (svref vector j) do
            (if (funcall predicate vector-j vector-i)
                (setf (svref vector j) vector-i)))))

(declaim (ftype (function (simple-vector function array-index array-index
                                          array-index array-index array-index)
                          (values t array-index &optional))
                mean-of-five))

(defun mean-of-five (vector predicate d a b c e)
  (declare (type array-index d a b c e))
  ;; First, make sure that va <= vb <= vc
  (let ((vd (svref vector d)) (va (svref vector a))
        (vb (svref vector b)) (vc (svref vector c))
        (ve (svref vector e)))
    (if (funcall predicate vb va)
        (if (funcall predicate vc va)
            (if (funcall predicate vb vc)
                ;; vb < vc < va
                (progn (rotatef va vb vc)
                       (rotatef a b c))
                ;; vc <= vb < va
                (progn (rotatef va vc)
                       (rotatef a c)))
            ;; vb < va <= vc
            (progn (rotatef va vb)
                   (rotatef a b)))
        (when (funcall predicate vc vb)
          (if (funcall predicate vc va)
              ;; vc < va <= vb
              (progn (rotatef va vc vb)
                     (rotatef a c b))
              ;; va <= vc < vb
              (progn (rotatef vb vc)
                     (rotatef b c)))))
    ;; Next, make sure vd <= ve
    (when (funcall predicate ve vd)
      (progn (rotatef vd ve)
             (rotatef d e)))
    ;; Finally, finds the mean, like in mergesort
    (if (funcall predicate va vd)
        (if (funcall predicate vb vd)
            (if (funcall predicate vc vd)
                (values vc c)
                (values vd d))
            (if (funcall predicate vb ve)
                (values vb b)
                (values ve e)))
        (if (funcall predicate va ve)
            (if (funcall predicate vb ve)
                (values vb b)
                (values ve e))
            (values va a)))))

#+nil
(defun mean-of-three (predicate a b c)
  (declare (type function predicate))
  (if (funcall predicate b a)
      (if (funcall predicate b c)
          ;; b < c and b < a
          (if (funcall predicate a c) a c)
          ;; c <= b < a
          b)
      (if (funcall predicate a c)
          ;; a < c and a <= b
          (if (funcall predicate b c) b c)
          ;; c <= a <= b
          a)))
