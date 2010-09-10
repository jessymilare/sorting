(in-package :sorting-benchmark)

#+(or sbcl cmucl)
(defun bench-time (function)
  (let (old-run-utime new-run-utime
        old-run-stime new-run-stime
        old-real-time new-real-time
        old-page-faults new-page-faults
        old-bytes-consed new-bytes-consed)
    (multiple-value-setq
        (old-run-utime old-run-stime old-page-faults old-bytes-consed)
      #+sbcl (sb-impl::time-get-sys-info)
      #+cmucl (lisp::time-get-sys-info))
    (setq old-real-time (get-internal-real-time))
    (funcall function)
    (multiple-value-setq
        (new-run-utime new-run-stime new-page-faults new-bytes-consed)
      #+sbcl (sb-impl::time-get-sys-info)
      #+cmucl (lisp::time-get-sys-info))
    (setq new-real-time (get-internal-real-time))
    ;; returns real user sys consed
    (values
     (max (/ (- new-real-time old-real-time)
             (float internal-time-units-per-second))
          0.0)
     (max (/ (- new-run-utime old-run-utime) 1000000.0) 0.0)
     (max (/ (- new-run-stime old-run-stime) 1000000.0) 0.0)
     (max (- new-bytes-consed old-bytes-consed) 0))))

#+clisp
(defun bench-time (function)
  (labels ((merge-2-values (val1 val2)
             (if (< internal-time-units-per-second 1000000)
                 ;; TIME_1: AMIGA, DOS, OS/2, UNIX_TIMES
                 (dpb val1 (byte 16 16) val2)
                 ;; TIME_2: UNIX sonst, WIN32
                 (+ (* val1 internal-time-units-per-second) val2)))
           (secs (v1 v2 v3 v4)
             (/ (- (merge-2-values v1 v2)
                   (merge-2-values v3 v4))
                (float internal-time-units-per-second))))
    (multiple-value-bind (new-real1 new-real2 new-run1 new-run2
                                    new-gc1 new-gc2 new-space1 new-space2
                                    new-gccount)
        (sys::%%time)
      (funcall function)
      (multiple-value-bind (old-real1 old-real2 old-run1 old-run2
                                      old-gc1 old-gc2 old-space1 old-space2
                                      old-gccount)
          (sys::%%time)
        ;; returns real user sys consed
        (values (secs old-real1 old-real2 new-real1 new-real2)
                (secs old-run1 old-run2 new-run1 new-run2)
                -1.0
                (- old-gccount new-gccount))))))

;; A generic timing function that depends on GET-INTERNAL-RUN-TIME
;; and GET-INTERNAL-REAL-TIME returning sensible results.
#-(or sbcl cmucl clisp)
(unless (fboundp 'bench-time)
  (defun bench-time (function)
    (let (before-real after-real before-user after-user)
      (setq before-user (get-internal-run-time))
      (setq before-real (get-internal-real-time))
      (funcall function)
      (setq after-user (get-internal-run-time))
      (setq after-real (get-internal-real-time))
      ;; return real user sys consed
      (values (/ (- after-real before-real)
                 (float internal-time-units-per-second))
              (/ (- after-user before-user)
                 (float internal-time-units-per-second))
              -1.0 -1.0))))
