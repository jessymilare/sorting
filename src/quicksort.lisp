;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :sorting)

(defun quicksort-simple (vector predicate)
  #.+declare-for-sort+
  (labels
      ((%quicksort (start end)
         (declare (type array-length start end))
         (unless (<= (- end start) 1)
           (let* ((index (truncate (+ start end) 2))
                  (last (1- end))
                  (pivot (svref vector index))
                  (start* (or (position-if (lambda (x)
                                             (not (funcall predicate x pivot)))
                                           vector :start start :end index)
                              index)))
             (setf (svref vector index) (svref vector last))
             (loop for i from (1+ start*) below last
                for vector-i = (svref vector i)
                with j = start*
                do
                  (when (funcall predicate vector-i pivot)
                    (shiftf (svref vector i) (svref vector j) vector-i)
                    (incf j))
                finally (progn
                          (shiftf (svref vector last) (svref vector j) pivot)
                          (%quicksort start j)
                          (%quicksort (1+ j) end)))))))
    (%quicksort 0 (length vector))
    vector))

;;; Interesting, but probably useless against mergesort
(defun quicksort-stable (vector predicate)
  #.+declare-for-sort+
  (let* ((length (length vector))
         (buffer (make-array length)))
    (labels
        ((%quicksort (start end)
           (declare (type array-length end)
                    (type array-index start)
                    (type simple-vector vector buffer))
           (unless (<= (- end start) 1)
             (let* ((index (truncate (+ start end) 2))
                    (pivot (svref vector index))
                    (start* (or (position-if (lambda (x)
                                               (not (funcall predicate x pivot)))
                                             vector :start start :end index)
                                index)))
               (setf (svref buffer 0) (svref vector start*))
               (loop for i from (1+ start*) below end
                  for vector-i = (svref vector i)
                  with j = start* and k = 1
                  do
                    (if (funcall predicate vector-i pivot)
                        (setf (svref vector j) vector-i j (1+ j))
                        (setf (svref buffer k) vector-i k (1+ k)))
                  finally
                    (progn
                      (loop for n from j
                         for m below k do
                           (setf (svref vector n) (svref buffer m)))
                      (when (= j start)
                        ;; Defective case, we have to make
                        ;; at least one step to go further.
                        ;; This step is from bubble-sort.
                        (loop for j from (1+ start) below end
                           with prev = (svref vector start)
                           for cur = (svref vector j) do
                             (if (funcall predicate cur prev)
                                 (setf (svref vector (1- j)) cur
                                       (svref vector j) prev)
                                 (setf prev cur)))
                        (decf end))
                      (%quicksort start j)
                      (%quicksort j end)))))))
      (%quicksort 0 length)
      vector)))

(defun %mean-of-means (vector predicate start end)
  #.+declare-for-sort+
  (declare (type array-length end)
           (type array-index start))
  (let ((length (- end start)))
    (cond
      ((>= length 25)
       (do ((end* (- end 5))
            (i start)
            (j start (+ 1 j)))
           ((> i end*)
            (multiple-value-bind (elt pos)
                (%select-nth (truncate (+ start j) 2) vector predicate
                             start j)
              (values elt pos j)))
         (declare (type array-length j i))
         (multiple-value-bind (elt pos)
             (mean-of-five vector predicate i (incf i)
                           (incf i) (incf i) (incf i))
           (incf i)
           (shiftf (svref vector pos) (svref vector j) elt))))
      ((>= length 5)
       (multiple-value-bind (elt pos)
           (mean-of-five vector predicate start (incf start)
                         (incf start) (incf start) (incf start))
         (unless (= start pos)
           (shiftf (svref vector pos) (svref vector start) elt))
         (values elt start start)))
      (t (values (svref vector start) start start)))))

(defun %select-nth (n vector predicate start end)
  #.+declare-for-sort+
  (declare (type array-length end)
           (type array-index start n))
  (multiple-value-bind (pivot index start-i)
      (%mean-of-means vector predicate start end)
    (declare (type array-index index start-i))
    (let ((last (1- end)))
      (cond
        ((< n index)
         (if (= index (1+ start))
             (values (svref vector start) start)
             (%select-nth n vector predicate start index)))
        (t
         (setf (svref vector index) (svref vector last))
         (loop for i from start-i below last
            for vector-i = (svref vector i)
            with j = index
            do
              (when (funcall predicate vector-i pivot)
                (shiftf (svref vector i) (svref vector j) vector-i)
                (incf j))
            finally (progn
                      (shiftf (svref vector last) (svref vector j) pivot)
                      (return
                        (cond
                          ((< n j)
                           (if (= (1+ start) j)
                               (values (svref vector start) start j end)
                               (%select-nth n vector predicate start j)))
                          ((> n j)
                           (incf j)
                           (if (= (1+ j) end)
                               (values (svref vector j) j j end)
                               (%select-nth n vector predicate j end)))
                          (t (values pivot j j end)))))))))))

(defun quicksort-with-mean (vector predicate)
  #.+declare-for-sort+
  (labels
      ((%quicksort (start end)
         (declare (type array-length end)
                  (type array-index start))
         (unless (<= (- end start) 1)
           (multiple-value-bind (pivot index start-i)
               (%mean-of-means vector predicate start end)
             (let ((last (1- end)))
               (declare (type array-index index))
               (setf (svref vector index) (svref vector last))
               (loop for i from start-i below last
                  for vector-i = (svref vector i)
                  with j = index
                  do
                    (when (funcall predicate vector-i pivot)
                      (shiftf (svref vector i) (svref vector j) vector-i)
                      (incf j))
                  finally (progn
                            (shiftf (svref vector last) (svref vector j) pivot)
                            (%quicksort start j)
                            (%quicksort (1+ j) end))))))))
    (%quicksort 0 (length vector))
    vector))
