;;; Code inspired by cl-bench

(in-package :sorting-benchmark)

(defvar *bench-groups* (make-hash-table :test 'eq))
(defvar *current-bench-group* nil)

(defclass benchmark ()
  ((name :accessor name-of :initarg :name)
   (exclude :accessor exclude-of :initarg :exclude :initform nil)
   (results :accessor results-of :initarg :results :initform nil)))

(defclass bench-unit (benchmark)
  ((setup :accessor setup-of :initarg :setup :type function)))

(defclass bench-group (benchmark)
  ((toplevel :accessor toplevel-p :initarg :toplevel :initform nil)
   (functions :accessor functions-of :initarg :functions :initform nil)
   (runs :accessor runs-of :initarg :runs :initform  1 :type fixnum)
   (members :accessor members-of :initarg :members :initform nil)
   (default-class :accessor default-class-of :initarg :default-class
                  :initform 'bench-unit :allocation :class)))

(defmethod functions-of ((bench bench-unit))
  (when *current-bench-group*
    (functions-of *current-bench-group*)))

(defmethod runs-of ((bench bench-unit))
  (when *current-bench-group*
    (runs-of *current-bench-group*)))

(defgeneric bench-creator (type parameters body))

(defmethod bench-creator ((type symbol) parameters body)
  (bench-creator #-closer-mop (make-instance type)
                 #+closer-mop (let ((class (find-class type)))
                                (closer-mop:finalize-inheritance class)
                                (closer-mop:class-prototype class))
                 parameters body))

(defmethod bench-creator ((type bench-group) parameters body)
  (with-gensyms (bench)
    `(let ((,bench (make-instance ',(class-name (class-of type))
                                  ,@parameters)))
       (setf (members-of ,bench)
             (list*
              ,@(mapcar (curry #'apply
                               (lambda (name class arguments &rest gbody)
                                 (bench-creator
                                  (or class (default-class-of type))
                                  `(:name ',name ,@arguments)
                                  gbody)))
                        body)
              (members-of ,bench)))
       ,bench)))

;;; Keeping my sanity when creating bench-units
(defmacro test-runner (bindings &body body)
  `(lambda (-test-function-name-)
     (declare (ignorable -test-function-name-)
              (optimize speed))
     (let* ((-test-function- (ignore-errors
                               (coerce -test-function-name- 'function)))
            ,@bindings)
       (declare (ignorable -test-function-))
       ,@body)))

(defmethod bench-creator ((type bench-unit) parameters body)
  (with-gensyms (bench)
    `(let ((,bench (make-instance ',(class-name (class-of type))
                                  ,@parameters)))
       (setf (setup-of ,bench)
             (lambda (-benchmark-)
               (declare (ignorable -benchmark-))
               ,@body))
       ,bench)))

(defmacro def-bench (name class (&rest arguments) &body body)
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *bench-groups*)
           ,(bench-creator (or class 'bench-group)
                           `(:name ',name :toplevel t ,@arguments)
                           body))
     ',name))

(defun find-bench (name)
  (gethash name *bench-groups*))

(defgeneric run-benchmark (benchmark &key &allow-other-keys)
  (declare (optimize speed (space 0)))
  (:method ((benchmark symbol) &rest all-keys &key)
    (apply #'run-benchmark (find-bench benchmark) all-keys)))

(defmethod run-benchmark ((benchmark bench-unit) &key)
  (declare (optimize speed (space 0)))
  (let ((results (mapcar
                  (lambda (fname)
                    (list fname 0.0 0.0 0.0 0))
                  (functions-of benchmark))))
    (dotimes (i (the fixnum (runs-of benchmark)))
      (let ((runner (funcall (the function
                               (setup-of benchmark)) benchmark)))
        (mapcar (lambda (fname result)
                  (tg:gc)
                  (setf (cdr result)
                        (mapcar #'+
                                (cdr result)
                                (multiple-value-list
                                 (bench-time (funcall (the function runner)
                                                      fname))))))
                (functions-of benchmark) results)))
    (setf (results-of benchmark) results)))

(defmethod run-benchmark ((benchmark bench-group) &rest all-keys &key)
  (declare (optimize speed (space 0)))
  (let ((*current-bench-group* benchmark))
    (setf (results-of benchmark)
          (loop for member in (members-of benchmark)
             collecting (cons (name-of member)
                              (apply #'run-benchmark member all-keys))))))

(defun relative-results-header (stream functions widths)
  (format stream "~:{~VT~A~}~%"
          (mapcar #'list widths functions)))

(defun calculate-widths (indentation functions)
  (cons indentation
        (mapcar (let ((acc (+ 3 indentation)))
                  (lambda (fname)
                    (incf acc (max 10 (+ 2 (length (string fname)))))))
                (butlast functions))))

(defgeneric report-results (benchmark place &key &allow-other-keys)
  (:method ((benchmark symbol) place &rest all-keys &key)
    (apply #'report-results (find-bench benchmark) place all-keys))
  (:method (benchmark (place symbol) &rest all-keys &key)
    (ecase place
      ((nil) (with-output-to-string (stream)
               (apply #'report-results benchmark stream all-keys)))
      ((t) (apply #'report-results benchmark *standard-output* all-keys)))))

(defmethod report-results :before ((benchmark bench-group) (stream stream)
                                   &key (indentation 0))
  (format stream "~VTGroup ~A~%~%" indentation (name-of benchmark)))

(defmethod report-results ((benchmark bench-group) (stream stream)
                           &key (indentation 0)
                           (results (results-of benchmark)))
  (let* ((functions (functions-of benchmark))
         (name-width (+ 5 (reduce #'max
                                  (mapcar (compose #'length #'string #'name-of)
                                          (members-of benchmark))
                                  :initial-value 10)))
         (widths (calculate-widths (+ indentation name-width) functions))
         (bench-units (remove-if-not (rcurry #'typep 'bench-unit)
                                        (members-of benchmark)))
         (bench-groups (remove-if-not (rcurry #'typep 'bench-group)
                                      (members-of benchmark))))
    (flet ((report-results-unit (benchmark results)
             (let* ((relative (cadr (assoc (car functions) results)))
                    (zerop (zerop relative)))
               (if zerop (setf relative 1))
               (format
                stream "~VT~A~VT[~,3Fs]~:{~VT~:[~,3F~;[~,3Fs]~]~}~%"
                (+ 2 indentation) (name-of benchmark)
                (car widths) (if zerop 0.0 relative)
                (mapcar (lambda (width function maybe-result)
                          (list (if zerop (- width 3) width) zerop
                                (/ (cadr (if (eq (car maybe-result) function)
                                             maybe-result
                                             (assoc function results)))
                                   relative)))
                        (rest widths) (rest functions) (rest results))))))
      (when bench-units
        (relative-results-header stream functions widths))
      (loop for bench in bench-units
         for result-cons = (member (name-of bench) results :key #'car)
         then (or (member (name-of bench) result-cons :key #'car)
                  (member (name-of bench) results :key #'car)) do
         (report-results-unit bench (cdar result-cons)))
      (terpri stream)
      (loop for bench in bench-groups
         for result-cons = (member (name-of bench) results :key #'car)
         then (or (member (name-of bench) (cdr result-cons) :key #'car)
                  (member (name-of bench) results :key #'car)) do
         (report-results bench stream :indentation (+ 2 indentation)
                         :results (cdar result-cons)))
      (terpri stream)
      (when (toplevel-p benchmark)
        (force-output stream)))))
