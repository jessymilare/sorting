
(in-package :sorting-benchmark)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass sort-bench (bench-unit)
   ())

  (defclass sort-bench-group (bench-group)
    ((default-class :initform 'sort-bench :allocation :class)
     (size :accessor size-of :initarg :size)
     (copies :accessor copies-of :initarg :copies)
     (predicate :accessor predicate-of :initarg :predicate))))

(defun copy-simple-vector (vector)
  (let ((copy (make-array (length vector))))
    (dotimes (i (length vector))
      (setf (svref copy i) (svref vector i)))
    copy))

(defmacro bench-sorter-creator (element-creator
                                &key array-specs optimization size)
  (with-gensyms (parent vector predicate list copy gsize)
    `(let* ((,parent *current-bench-group*)
            (,gsize (or ,size (size-of ,parent)))
            (,vector (make-array ,gsize ,@array-specs))
            (,predicate (symbol-function (predicate-of ,parent))))
       (declare (type array-length ,gsize))
       (dotimes (-index- ,gsize)
         (setf (aref ,vector -index-)
               (the fixnum ,element-creator)))
       (test-runner ((,list (loop repeat (the fixnum (copies-of ,parent))
                               collect (copy-simple-vector ,vector))))
         (lambda ()
           (declare (optimize ,@optimization))
           (dolist (,copy ,list)
             (funcall -test-function- ,copy ,predicate)))))))

(defun my< (a b)
  (dolist (bit '(1 2 4 8 16 32 64 128 256) (< a b))
    (if (> (logand a bit)
           (logand b bit))
        (return nil))
    (if (< (logand a bit)
           (logand b bit))
        (return t))))

(def-bench numeric-simple-vectors sort-bench-group ()
  (simple-vector-range-1 nil ()
    (bench-sorter-creator 0))
  (simple-vector-range-2 nil ()
    (bench-sorter-creator (random 2)))
  (simple-vector-range-100 nil ()
    (bench-sorter-creator (random 100)))
  (simple-vector-large-range nil ()
    (bench-sorter-creator (random most-positive-fixnum))))

(define-constant +all-sorting-functions+
    '(sort stable-sort
      insertion-sort bubble-sort selection-sort
      quicksort-simple quicksort-with-mean quicksort-stable
      merge-sort heapsort)
  :test 'equal)

(define-constant +internal-sorting-functions+
    '(quicksort-simple quicksort-with-mean quicksort-stable
      merge-sort heapsort
      insertion-sort bubble-sort selection-sort)
  :test 'equal)

(def-bench small-numeric-vectors sort-bench-group
    (:runs 10 :copies 100000 :predicate '<
           :functions +internal-sorting-functions+)
  (vector-2-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 2))
  (vector-4-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 4))
  (vector-6-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 6))
  (vector-8-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 8))
  (vector-10-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 10))
  (vector-12-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 12))
  (vector-14-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 14))
  (vector-16-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 16))
  (vector-18-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 18))
  (vector-20-elts nil ()
   (bench-sorter-creator (random most-positive-fixnum) :size 20)))
