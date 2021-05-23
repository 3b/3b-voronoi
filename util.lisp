(in-package #:3b-voronoi)


(defun next-float (f)
  (etypecase f
    (single-float
     (let ((b (float-features:single-float-bits f)))
       (float-features:bits-single-float (1+ b))))
    (double-float
     (let ((b (float-features:double-float-bits f)))
       (float-features:bits-double-float (1+ b))))))

(defun prior-float (f)
  (etypecase f
    (single-float
     (let ((b (float-features:single-float-bits f)))
       (float-features:bits-single-float (1- b))))
    (double-float
     (let ((b (float-features:double-float-bits f)))
       (float-features:bits-double-float (1- b))))))


(defun u32-priority (y)
  ;; calculate a d-f-p-q priority from float Y
  (let* ((f (float-features:single-float-bits
             (coerce y 'single-float)))
         (s (logbitp 31 f))
         ;; this maps +0 and -0 to same priority, but that's probably
         ;; OK?
         (r (if s
                ;; and negative in order by magnitude above that
                (+ #x80000000 (ldb (byte 31 0) f))
                ;; map positive #s in reverse order below #x8000000
                (- #x80000000 (ldb (byte 31 0) f)))))
    r))
