(in-package #:3b-voronoi)

(defclass sweep-edge (ray)
  ;; link to ray (or sweep-edge) in other direction if we started in
  ;; the middle of a voronoi edge, or a point if we started at a
  ;; voronoi vertex or edge of bounds (or NIL if point is at infinity
  ;; and wee haven't filled in bounds intersection yet)
  ((back :reader back :initform nil :initarg :back)
   ;; if we reached an endpoint (= voronoi vertex) it gets stored here
   (end :accessor end :initform nil)
   ;; focuses of arcs on either side of edge
   (left-focus :reader left-focus :initarg :left-focus)
   (right-focus :reader right-focus :initarg :right-focus)
   ;; any active circles for arcs bounded by this edge
   (left-circle :accessor left-circle :initform nil)
   (right-circle :accessor right-circle :initform nil)
   ;; negative, 0, or positive to distinguish which of multiple
   ;; possible intersections to use. negative for left intersection,
   ;; positive for right, 0 if there should be only 1 intersection
   (edge-type :reader edge-type :initarg :edge-type)))


(defun intersect-edge (edge y)
  (declare (optimize speed))
  (assert (and (left-focus edge) (right-focus edge)))
  (let* ((y (coerce y 'double-float))
         (left (left-focus edge))
         (right (right-focus edge)))
    (declare (type point left right)
             (type double-float y))
    (cond
      ((= (py left) (py right))
       ;; if both focus have same Y coord, intersection is always
       ;; midway between them so just return that
       (+ (/ (px left) 2d0)
          (/ (px right) 2d0)))
      ;; if one focus has same y as sweep line, intersection is above
      ;; that focus
      ((= (py left) y)
       (values (px left) :=r (px right)))
      ((= (py right) y)
       (values (px right) :=r (px left)))
      (t ;; general case
       (let* ((t1 (make-array 3 :element-type 'double-float))
              (t2 (make-array 3 :element-type 'double-float))
              (q1 (%quadratic left y t1))
              (q2 (%quadratic right y t2)))
         (declare (type (simple-array double-float (3)) t1 t2)
                  (dynamic-extent t1 t2))
        (multiple-value-bind (l r)
            (intersect-quadratic q1 q2)
          (declare (type (or null double-float) l r))
          (if r
              ;; 2 solutions, pick depending on direction of edge
              (let ((d (edge-type edge)))
                (declare (type (integer -2 2) d))
                ;; shouldn't have 2 solutions, and should only be 0 in
                ;; cases where the equal y-coord case above would have
                ;; handled it already
                (assert (not (zerop d)))
                (if (minusp d)
                    (values (min r l) :l (max r l))
                    (values (max r l) :r (min r l))))
              ;; single solution
              (progn
                ;; should only happen for vertical edges, which should
                ;; have been handled by equal y-coord case already
                (break "shouldn't get here? ~s ~s" l r)
                l))))))))

(defun before-edge-p (edge x y)
  (cond
    ;; at edge of tree, left or right is NIL, so we always go other
    ;; direction from that
    ((not (right-focus edge))
     t)
    ((not (left-focus edge))
     nil)
    (t (< x (intersect-edge edge y)))))

