;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :sorting)

(defun merge-sort (vector predicate)
  #.+declare-for-sort+
  (let* ((length (length vector))
         (buffer (make-array length)))
    (labels
        ((%mergesort (dest start end buffer)
           (declare (type array-length end)
                    (type array-index start)
                    (type simple-vector vector buffer))
           (cond
             ((> (- end start) 1)
              (let ((middle (truncate (+ start end) 2)))
                (%mergesort buffer start middle dest)
                (%mergesort dest middle end buffer)
                (do ((k start (1+ k))
                     (i start)
                     (elt-i (svref buffer start))
                     (j middle)
                     (elt-j (svref dest middle)))
                    (nil)
                  (declare (type array-length i j k))
                  (cond
                    ((funcall predicate elt-i elt-j)
                     (setf (svref dest k) elt-i)
                     (when (= middle (incf i))
                       (return))
                     (setf elt-i (svref buffer i)))
                    (t
                     (setf (svref dest k) elt-j)
                     (when (= end (incf j))
                       (setf (svref dest (incf k)) elt-i)
                       (do ()
                           ((>= (incf k) end))
                         (setf (svref dest k) (svref buffer (incf i))))
                       (return))
                     (setf elt-j (svref dest j)))))))
             ((eq dest vector)
              nil)
             (t (loop for k from start below end do
                     (setf (svref dest k) (svref vector k)))))))
      (%mergesort vector 0 length buffer)
      vector)))

(defun heapsort (vector predicate)
  #.+declare-for-sort+
  (flet
      ((sift-down (pos elt end)
         (declare (type array-length end)
                  (type array-index pos))
         (loop for i of-type array-index = pos then j
            for j of-type array-index = (1+ (* i 2))
            while (< j end)
            for child = (svref vector j) do
              (if (and (< (1+ j) end)
                       (funcall predicate child (svref vector (1+ j))))
                  (setf child (svref vector (incf j))))
              (if (funcall predicate elt child)
                  (setf (svref vector i) child)
                  (progn
                    (setf (svref vector i) elt)
                    (return)))
            finally (setf (svref vector i) elt))))
    (let ((length (length vector)))
      (loop for i from (truncate (- length 2) 2) downto 0 do
           (sift-down i (svref vector i) length))
      (loop for n from (1- length) downto 0
           for elt = (shiftf (aref vector n) (aref vector 0)) do
           (sift-down 0 elt n)))
    vector))
