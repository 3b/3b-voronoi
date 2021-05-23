(in-package #:3b-voronoi)



(deftype point () '(simple-array single-float (2)))
(declaim (inline p px py (setf px) (setf py)))

(defun p (x y)
  (make-array 2 :element-type 'single-float
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))

(defun px (a) (aref a 0))
(defun py (a) (aref a 1))

(defun (setf px) (n a) (setf (aref a 0) n))
(defun (setf py) (n a) (setf (aref a 1) n))

(deftype dpoint () '(simple-array double-float (2)))
(declaim (inline dp dpx dpy (setf dpx) (setf dpy)))

(defun dp (x y)
  (make-array 2 :element-type 'double-float
                :initial-contents (list (coerce x 'double-float)
                                        (coerce y 'double-float))))

(defun dpx (a) (aref a 0))
(defun dpy (a) (aref a 1))

(defun (setf dpx) (n a) (setf (aref a 0) n))
(defun (setf dpy) (n a) (setf (aref a 1) n))


(defun dist (a b)
  (sqrt (+ (expt (- (px a) (px b)) 2)
           (expt (- (py a) (py b)) 2))))

(defun dist^2 (a b)
  (+ (expt (- (px a) (px b)) 2)
     (expt (- (py a) (py b)) 2)))

(declaim (inline point-order))
(defun point-order (a b)
  ;; descending Y, ascending X for now to match debugging display.
  ;; todo: test with other combinations
  (or (> (py a) (py b))
      (and (= (py a) (py b))
           (< (px a) (px b)))))

(defun quadratic (a1 yd)
  (declare (type point a1)
           (optimize speed))
  (let ((yd (coerce yd 'double-float))
        (xf (coerce (px a1) 'double-float))
        (yf (coerce (py a1) 'double-float)))
    (if (or (= (py a1) yd)
            ;; not sure if comparing before and after coerce is always
            ;; equivalent, so doing both to make sure
            (= yf yd))
        xf
        (let* (;; from https://jacquesheunis.com/post/fortunes-algorith/m
               ;; y = (/ (* 2 (- yf yd))) (- x xf)² + (/ (+ yf yd) 2)
               ;; c1 = (/ (* 2 (- yf yd)))
               ;; c2 = (/ (+ yf yd) 2)
               ;; y = c1 × (- x xf)² + c2
               ;; y = c1 × (+ x² (* -2 xf x) xf²) + c2
               ;; y = (+ (* c1 x²) (* -2 c1 xf x) (* c1 xf²) c2)
               ;;  a = c1
               ;;  b = (* -2 c1 xf)
               ;;  c = (+ c2 (* c1 xf xf))
               (c1 (/ (* 2 (- yf yd))))
               (c2 (/ (+ yf yd) 2))
               (a c1)
               (b (* -2 c1 xf))
               (c (+ c2 (* (expt xf 2) c1))))
          (make-array 3 :element-type 'double-float
                        :initial-contents (list a b c))))))

(defun eval-quadratic (q x)
  (let ((x2 (* x x)))
    (+ (* x2 (aref q 0))
       (* x (aref q 1))
       (aref q 2))))

(declaim (inline intersect-quadratic))
(defun intersect-quadratic (q1 q2)
  (declare (type (or double-float (simple-array double-float (3))) q1 q2)
           (optimize speed))
  ;; degenerate quadratics should be handled by caller
  (assert (not (or (numberp q1) (numberp q2))))
  (flet ((qf (a b c)
           (let ((d (- (expt b 2) (* 4 a c))))
             (cond
               ((minusp d)
                (values))
               ((zerop a) ;; linear, bx+c=0, bx=-c,
                (break "got linear quadratic?~% ~s ~s ~s" a b c)
                (- (/ c b)))
               ((zerop d)
                (/ b (* -2 a)))
               (t
                (let* ((rd (sqrt d))
                       (q (* -1/2 (if (minusp b)
                                      (- b rd)
                                      (+ b rd)))))
                  (values (/ q a) (/ c q))))))))
    (let* ((a1 (aref q1 0))
           (b1 (aref q1 1))
           (c1 (aref q1 2))
           (a2 (aref q2 0))
           (b2 (aref q2 1))
           (c2 (aref q2 2))
           (a3 (- a1 a2))
           (b3 (- b1 b2))
           (c3 (- c1 c2)))
      (declare (type double-float a1 b1 c1 a2 b2 c2 a3 b3 c3))
      (qf a3 b3 c3))))


(defun midpoint (a b)
  (dp (+ (* 0.5d0 (px a)) (* 0.5 (px b)))
      (+ (* 0.5d0 (py a)) (* 0.5 (py b)))))

(defun bisector-dir (a b)
  ;; dir from a to b = (ax-bx),(ay-by). rotated to get bisector =
  ;; -(ay-by),(ax-bx) = (by-ay),(ax-bx)
  (dp (- (coerce (py b) 'double-float) (coerce (py a) 'double-float))
      (- (coerce (px a) 'double-float) (coerce (px b) 'double-float))))




(defclass ray ()
  ;; a point on line containing the ray (might not be on the ray or
  ;; within a voronoi edge)
  ((ref :reader ref :initarg :ref)
   ;; point where ray started
   (start :Reader start :initarg :start)
   ;; non-normalized vector in direction of ray
   (dir :reader dir :initarg :dir)))


(defun need-circle (a b c)
  (when (eql a c)
    ;; arc formed by B can't collapse if arcs on either side are from
    ;; same site
    (return-from need-circle nil))
  ;; intersection of bisectors of ab and bc is to right of a->b and
  ;; b->c
  (let* ((x1 (- (px b) (px a)))
         (y1 (- (py b) (py a)))
         (x2 (- (px c) (px b)))
         (y2 (- (py c) (py b)))
         (a (atan (- (* x1 y2) (* y1 x2))
                  (+ (* x1 x2) (* y1 y2)))))
    (minusp a)))

(defun intersect-rays (a b)
  (let* ((p1 (ref a))
         (p2 (ref b))
         (d1 (dir a))
         (d2 (dir b))
         (x1 (px p1))
         (y1 (py p1))
         (x2 (+ x1 (px d1)))
         (y2 (+ y1 (py d1)))
         (x3 (px p2))
         (y3 (py p2))
         (x4 (+ x3 (px d2)))
         (y4 (+ y3 (py d2)))
         (x1-x2 (- x1 x2))
         (x3-x4 (- x3 x4))
         (y1-y2 (- y1 y2))
         (y3-y4 (- y3 y4))
         (d (- (* x1-x2 y3-y4)
               (* y1-y2 x3-x4)))
         (x1y2 (* x1 y2))
         (y1x2 (* y1 x2))
         (x1y2-y1x2 (- x1y2 y1x2))
         (x3y4 (* x3 y4))
         (y3x4 (* y3 x4))
         (x3y4-y3x4 (- x3y4 y3x4)))
    (unless (zerop d)
      (let ((x (/ (- (* x1y2-y1x2 x3-x4)
                     (* x1-x2 x3y4-y3x4))
                  d))
            (y (/ (- (* x1y2-y1x2 y3-y4)
                     (* y1-y2 x3y4-y3x4))
                  d)))
        (dp x y)))))

