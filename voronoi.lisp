#++ (ql:quickload '(3b-voronoi))
(in-package #:3b-voronoi)

;; refs

;; fortune 1987 A Sweepline Algorithm for Voronoi Diagrams
;;   https://gi.cebitec.uni-bielefeld.de/_media/teaching/2013summer/936fortune-1987-voronoi.pdf

;; https://jacquesheunis.com/post/fortunes-algorithm/
;; https://jacquesheunis.com/post/fortunes-algorithm-implementation/

;; An Efficient Implementation of Fortune's Plane-Sweep Algorithm for Voronoi Diagrams
;;   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.83.5571&rep=rep1&type=pdf

(defparameter *dump* nil)
#++
(setf *dump* t)
;; for debugging
(defun dump-tree (st &key y)
  (let ((y (or y
               (loop for e = (table-start st) then (next e)
                     while (and e (right-focus e))
                     minimize (py (right-focus e))))))
    (labels ((i (n)
               (cond
                 ((and (left-focus n) (right-focus n))
                  (if (and (>= (py (left-focus n)) y)
                           (>= (py (right-focus n)) y))
                      (multiple-value-list
                       (intersect-edge n y))
                      (list :below-sweep)))))
             (dump-node (n indent)
               (when (tree-left n)
                 (dump-node (tree-left n) (concatenate 'string "  " indent))
                 (assert (eql (tree-up (tree-left n)) n)))
               (format t "~&~a ~s :: ~s,~s = [~{~,3f~^ ~}]~%" indent
                       (cond
                         ((minusp (edge-type n)) "L")
                         ((plusp (edge-type n)) "R")
                         ((zerop (edge-type n)) "D")
                         (t "S?"))
                       (left-focus n)
                       (right-focus n)
                       (i n))
               (when (tree-right n)
                 (dump-node (tree-right n) (concatenate 'string "  " indent))
                 (assert (eql (tree-up (tree-right n)) n)))))
      (when (and st (root st))
        (format t "~& ---- dump tree @ y = ~s~%" y)
        (dump-node (root st) "")
        (let ((n (table-start st)))
          (format t "  == ~%")
          (loop do (format t "   ~s :: ~s,~s = [~{~f~^ ~}]~%"
                           (cond
                             ((minusp (edge-type n)) "L")
                             ((plusp (edge-type n)) "R")
                             ((zerop (edge-type n)) "D")
                             (t "S?"))
                           (left-focus n)
                           (right-focus n)
                           (i n))
                   (setf n (next n))
                while n))))))

(defclass voronoi-state ()
  ((beachline :accessor beachline :initform (make-instance 'sweep-table))
   (q :reader q :initform (q:make-queue))
   (sites :accessor sites :initarg :sites)
   (bmin :reader bmin :initarg :bmin)
   (bmax :reader bmax :initarg :bmax)
   (vertices :reader vertices :initform (make-array 16 :adjustable t
                                                       :fill-pointer 0))
   (edges :reader edges :initform (make-array 16 :adjustable t
                                                 :fill-pointer 0))
   ;; list of suspicious edges that go to infinity but cross most of
   ;; the bounds, indicating they might be caused by a bug
   (cross-edges :accessor cross-edges :initform nil)))

(defun peek-next-event (state)
  (cond
    ((not (sites state))
     (q:peek (q state)))
    ((zerop (q:size (q state)))
     (car (sites state)))
    ((point-order (car (sites state))
                  (pe (q:peek (q state))))
     (car (sites state)))
    (t
     (q:peek (q state)))))

(defun next-event (state)
  (cond
    ((not (sites state))
     (multiple-value-bind (v p h) (q:dequeue (q state))
       (declare (ignorable p h))
       #++(when p
            (remove-circle-from-edges v h))
       v))
    ((zerop (q:size (q state)))
     (pop (sites state)))
    ((not (point-order (pe (q:peek (q state))) 
                       (car (sites state))))
     (pop (sites state)))
    (t
     (multiple-value-bind (v p h) (q:dequeue (q state))
       (declare (ignorable p h))
       #++(when p
            (remove-circle-from-edges v h))
       v))))

(defun make-edge-pair (hit site start)
  (let* ((mid (midpoint hit site))
         (dir (bisector-dir hit site))
         (-dir (dp (- (px dir)) (- (py dir))))
         (a (make-instance 'sweep-edge :left-focus hit :right-focus site
                                       :ref mid :start start :dir dir
                                       :edge-type -1))
         (b (make-instance 'sweep-edge :left-focus site :right-focus hit
                                       :ref mid :start start :dir -dir
                                       :back a
                                       :edge-type 1)))
    (setf (slot-value a 'back) b)
    (values a b)))

(defun split-arc (state a b site)
  (when *dump*
    (format t "insert ~s between ~s,~s~%" site a b)
    (when a
      (format t "  = ~s (~s, ~s)~%"
              a (left-focus a) (right-focus a)))
    (when b
      (format t "  = ~s (~s, ~s)~%"
              b (left-focus b) (right-focus b))))
  ;; edges should bound a single arc
  (when (and a b)
    (assert (or (and (right-focus a) (left-focus b))))
    (assert (eql (right-focus a) (left-focus b))))
  (let ((hit (if a (right-focus a) (left-focus b))))
    (cond
      ;; if focuses have same Y coordinate, we don't want to actually
      ;; split the hit arc, since there is only 1 intersection between
      ;; the 2 parabolas
      ((= (py hit) (py site))
       (cond
         ((< (px site) (px hit))
          (when *dump*
            (format t "  insert down edge left~%"))
          (let* ((l (left-focus a))
                 (mid (midpoint hit site))
                 ;; start ray at bounds
                 (start (dp (px mid) (py (bmin state))))
                 (e1 (make-instance 'sweep-edge
                                    :left-focus l
                                    :right-focus site
                                    :ref mid
                                    :start start
                                    :dir (when l
                                           (bisector-dir l site))
                                    :back start
                                    :edge-type -1))
                 (e2 (make-instance 'sweep-edge
                                    :left-focus site
                                    :right-focus hit
                                    :ref mid
                                    :start start
                                    :dir (dp 0d0 1d0)
                                    :back start
                                    :edge-type 0)))
            (insert-before e1 b)
            (insert-before e2 b)
            (delete-node a)
            (values e1 e2)))
         ((> (px site) (px hit))
          (when *dump*
            (format t "  insert down edge right~%"))
          (let* ((r (right-focus b))
                 (mid (midpoint hit site))
                 ;; start ray at bounds
                 (start (dp (px mid) (py (bmax state))))
                 (e1 (make-instance 'sweep-edge
                                    :left-focus hit
                                    :right-focus site
                                    :ref mid
                                    :start start
                                    :dir (dp 0d0 1d0)
                                    :back start
                                    :edge-type 0))
                 (e2 (make-instance 'sweep-edge
                                    :left-focus site
                                    :right-focus r
                                    :ref mid
                                    :start start
                                    :dir (when r
                                           (bisector-dir site r))
                                    :back start
                                    :edge-type 1)))
            (delete-node b)
            (let ((n2 (insert-after e2 a))
                  (n1 (insert-after e1 a)))
              (values n1 n2))))
         (t
          (error "duplicate sites ~s ~s?" site hit))))
      ;; normal case, split HIT and add left/right arcs for new site
      (t
       (let* ((q (quadratic hit (py site)))
              (x (px site))
              (y (eval-quadratic q x)))
         (multiple-value-bind (le re) (make-edge-pair hit site (dp x y))
           (if a
               (values (insert-after le a)
                       (insert-after re le))
               (values (insert-before le b)
                       (insert-after re le)))))))))

(defun %make-circle (a b)
  (assert (and (left-focus a)
               (right-focus a)
               (eql (right-focus a) (left-focus b))
               (right-focus b)))
  (let ((center (intersect-rays a b)))
    (when center
      (let* ((r (dist center (right-focus a)))
             (y (- (py center) r)))
        (make-instance 'circle-event
                       :pe (dp (px center) y)
                       :c center
                       :r r
                       :left-edge a
                       :right-edge b)))))

(defun %add-circle-event (state a b c)
  (when *dump*
    (format t "----------------~%")
    (format t "add circle @ ~s, ~s, ~s:~%"
            (left-focus a) (right-focus a) (right-focus b))
    (format t " between ~s ~s~%" a b)
    (format t "  ~s + ~s~%  ~s + ~s~%"
            (ref a) (dir a)
            (ref b) (dir b))
    (format t " = ~s @ ~s~% => ~s~%" (r c) (c c) (pe c)))
  (flet ((live-circle-p (x)
           (and x (q:handle-priority (q state) x))))
    (let ((h (q:enqueue (q state) c (u32-priority (py (pe c))))))
      (assert (not (live-circle-p (right-circle a))))
      (setf (right-circle a) h)
      (assert (not (live-circle-p (left-circle b))))
      (setf (left-circle b) h))))

(defun %maybe-add-circle (state left right sweep)
  (when (need-circle (left-focus left) (right-focus left) (right-focus right))
    (let ((c (%make-circle left right)))
      (when *dump*
        (format t " -- %maybe-add-circle~%   ~s ~s ~s~%"
                (left-focus left) (right-focus left) (right-focus right)))
      (when (and c *dump*)
        (format t "    y ~s < sweep ~s? ~s~%" (py (pe c)) sweep
                (and c (<= (py (pe c)) sweep))))
      (when (and c (> (py (pe c)) sweep))
        ;; if we made a circle at a site, by definition circle must
        ;; extend to that point (fixme: figure out if this is correct
        ;; when called from circle event?)
        (setf (py (pe c)) (coerce sweep 'double-float)))
      (when c
        #++ (and c (<= (py (pe c)) sweep))
        (%add-circle-event state left right c)))))

(defun maybe-add-circles (state left-node right-node sweep)
  (assert (not (right-circle left-node)))
  (assert (not (left-circle right-node)))
  (when *dump*
    (format t " ------------~% maybe-add-circles~%")
    (format t "~s,~s:~%~s, ~s~% -- ~s, ~s~%"
            left-node right-node
            (left-focus left-node) (right-focus left-node)
            (left-focus right-node) (right-focus right-node)))
  (when (or (eql (prev left-node) right-node)
            (eql (next right-node) left-node))
    (break "backwards?"))
  (when (and (prev left-node) (left-focus (prev left-node)))
    (when *dump*
      (format t "  -- maybe add left ~s~%"
              (left-focus (prev left-node))))
    (%maybe-add-circle state (prev left-node) left-node sweep))
  (when (and (next right-node) (right-focus (next right-node)))
    (when *dump*
      (format t "  -- maybe add right ~s~%"
              (right-focus (next right-node))))
    (%maybe-add-circle state right-node (next right-node) sweep)))


(defun maybe-remove-circles (q a b)
  (assert (eql (right-circle a) (left-circle b)))
  (when (or (right-circle a) (left-circle b))
    (when *dump*
      (let ((c (cdr (right-circle a))))
        (format t "xxxxxxxxxxxxxxx~% maybe-remove-circles ~s:~%  ~s @ ~s~% pe ~s~%"
                c
                (r c) (c c) (pe c))))
    (q:delete q (right-circle a))
    (setf (right-circle a) nil)
    (setf (left-circle b) nil)))

(defun end-ray (state ray at)
  (assert (not (end ray)))
  (setf (end ray) at)
  (etypecase (back ray)
    (null
     (break "no back on ray ~s?" ray))
    ((or point dpoint)
     (let ((i (vector-push-extend at (vertices state)))
           (j (vector-push-extend (back ray) (vertices state))))
       (vector-push-extend (list i j) (edges state))))
    (sweep-edge
     (when (end (back ray))
       (let ((i (vector-push-extend at (vertices state)))
             (j (vector-push-extend (end (back ray)) (vertices state))))
         (vector-push-extend (list i j) (edges state)))))))

(defun handle-circle (state circle)
  (let ((l (left-edge circle))
        (r (right-edge circle)))
    ;; remove arc if it still exists
    (unless (or (eql (tree-up l) :deleted)
                (eql (tree-up r) :deleted))
      (assert (and (right-circle l)
                   (eql circle (q:handle-data (q state) (right-circle l)))))
      (assert (and (left-circle r)
                   (eql circle (q:handle-data (q state) (left-circle r)))))

      (when *dump*
        (format t "====================~%")
        (format t "handle circle~% ~s @ ~s~% (~s)~%"
                (r circle) (c circle) (pe circle)))
      (let* ((prev (prev l))
             (left-focus (right-focus prev))
             (next (next r))
             (right-focus (left-focus next)))
        (when *dump*
          (format t " delete arc around ~s~%   between ~s, ~s~%"
                  (right-focus l) left-focus right-focus)
          (format t "  delete node ~s~%" l)
          (format t "  delete node ~s~%" r))
        (end-ray state l (c circle))
        (end-ray state r (c circle))
        (maybe-remove-circles (q state) prev l)
        (maybe-remove-circles (q state) r next)
        (delete-node l)
        (delete-node r)
        (let ((new (make-instance 'sweep-edge
                                  :left-focus left-focus
                                  :right-focus right-focus
                                  :edge-type (if (point-order left-focus right-focus)
                                                 -1 1)
                                  :ref (midpoint left-focus right-focus)
                                  :start (c circle)
                                  :dir (bisector-dir left-focus right-focus)
                                  :back (c circle))))
          (when *dump*
            (format t "insert node ~s @ ~s ~s~% start ~s, dir ~s~%"
                    new left-focus right-focus
                    (start new) (dir new)))
          (if prev
              (insert-after new prev)
              (insert-before new next))

          ;; and add new circle events on either side if needed
          (when (left-focus prev)
            (when *dump*
              (format t "add left circle ~s, ~s?~%" prev new))
            (%maybe-add-circle state prev new (py (pe circle))))
          (when (right-focus next)
            (when *dump*
              (format t "add right circle ~s, ~s?~%" new next))
            (%maybe-add-circle state new next (py (pe circle)))))))))



(defun add-site (state site)
  (let ((table (beachline state))
        (x (px site))
        (y (py site)))
    (if (not (root table))
        ;; first arc
        (progn
          (setf (root table) (make-instance 'sweep-table-node
                                            :ref nil :dir nil
                                            :left-focus nil :right-focus site
                                            :tree-up table
                                            :edge-type -1))
          (insert-after (make-instance 'sweep-edge
                                       :ref nil :dir nil
                                       :left-focus site :right-focus nil
                                       :edge-type 1)
                        (root table)))
        ;; subsequent arcs
        (multiple-value-bind (a b) (search-table table x y)
          (when *dump*
            (dump-tree table :y y))
          ;; remove circle events on arc being removed
          (when (and a b)
            (maybe-remove-circles (q state) a b))
          ;; add edges for new arc
          (multiple-value-bind (l r)
              (split-arc state a b site)
            ;; add a new circle events if needed
            (maybe-add-circles state l r (py site)))))
    (when *dump*
      (dump-tree table :y y))))

#++
(defun handle-circle (state circle))

(defun make-voronoi-state (points)
  (let* ((sites (coerce (sort (copy-seq points) 'point-order) 'list))
         (bmin (p (reduce 'min sites :key 'px)
                  (reduce 'min sites :key 'py)))
         (bmax (p (reduce 'max sites :key 'px)
                  (reduce 'max sites :key 'py))))
;;; sanity checks:
    ;; more than one site
    (assert (cdr sites))
    ;; more than 1 distinct site
    (assert (or (/= (px bmin) (px bmax))
                (/= (py bmin) (py bmax))))
    (make-instance 'voronoi-state :sites sites
                                  :bmin bmin :bmax bmax)))

(defun clip-ray (state ray)
  (when (and (dir ray) (not (end ray)))
    (let* ((x1 (px (bmin state)))
           (y1 (py (bmin state)))
           (x2 (px (bmax state)))
           (y2 (py (bmax state)))
           (rx (px (ref ray)))
           (ry (py (ref ray)))
           (sx (px (start ray)))
           (sy (py (start ray)))
           (dx (px (dir ray)))
           (dy (py (dir ray)))
           (e 0.0)
           (mx (/ (+ x1 x2) 2))
           (my (/ (+ y1 y2) 2)))
      (loop for i across (vertices state)
            for x = (px i)
            for y = (py i)
            when (< x x1)
              do (setf x1 x)
            when (< y y1)
              do (setf y1 y)
            when (> x x2)
              do (setf x2 x)
            when (> y y2)
              do (setf y2 y))
      (flet ((e (a b da db at)
               (+ a (* (/ da db) (- at b))))
             (in ()
               (and (<= (px (bmin state)) sx (px (bmax state)))
                    (<= (py (bmin state)) sy (py (bmax state))))))
        (cond
          ((and (zerop dx) (zerop dy))
           (break "degenerate ray? dx ~s dy ~s" dx dy))
          ((and (zerop dy) (minusp dx))
           (let ((i (end-ray state ray (dp x1 sy))))
             (when (and i (in) (> sx mx))
               (break "~s ~s" sx mx)
               i)))
          ((and (zerop dy) (plusp dx))
           (let ((i (end-ray state ray (dp x2 sy))))
             (when (and i (in) (< sx mx))
               (break "~s ~s" sx mx)
               i)))

          ((and (zerop dx) (minusp dy))
           (let ((i (end-ray state ray (dp sx y1))))
             (when (and i (in) (> sy my))
               (break "~s ~s" sy my)
               i)))
          ((and (zerop dx) (plusp dy))
           (let ((i (end-ray state ray (dp sx y2))))
             (when (and i (in) (< sy my))
               (break "~s ~s" sy my)
               i)))

          ((and (>= (abs dx) (abs dy))
                (minusp dx))
           (let ((i (end-ray state ray (dp x1 (e ry rx dy dx x1)))))
             (when (and i (in) (> sx mx))
               (break "~s ~s" sx mx)
               i)))
          ((and (>= (abs dx) (abs dy))
                (plusp dx))
           (let ((i (end-ray state ray (dp x2 (e ry rx dy dx x2)))))
             (when (and i (in) (< sx mx))
               (break "~s ~s" sx mx)
               i)))

          ((and (> (abs dy) (abs dx))
                (minusp dy))
           (let ((i (end-ray state ray (dp (e rx ry dx dy y1) y1))))
             (when (and i (in) (> sy my))
               (break "~s ~s" sy my)
               i)))
          ((and (> (abs dy) (abs dx))
                (plusp dy))
           (let ((i (end-ray state ray (dp (e rx ry dx dy y2) y2))))
             (when (and i (in) (< sy my))
               (break "~s ~s" sy my)
               i)))
          (t (break "?")))))))

(defun finish-edges (state)
  (assert (and (zerop (q:size (q state)))
               (not (sites state))))
  (loop for e = (table-start (beachline state)) then (next e)
        while e
        for c = (clip-ray state e)
        when c do (push c (cross-edges state)))
  (when (Cross-edges state)
    (break "cross ~s?" (cross-edges state)))
  (setf (root (beachline state)) nil))

(defun check-edges (state)
  ;; for debug/testing, make sure no edges intersect

  ;; todo: replace with per-cell checks instead of full n^2 search
  ;; when full graph is available
  (let ((ok t))
    (loop with e = (edges state)
          with v = (vertices state)
          for i below (1- (length e))
          do (flet ((check (a b)
                      (let* ((x1 (rational (px (aref v (car a)))))
                             (y1 (rational (py (aref v (car a)))))
                             (x2 (rational (px (aref v (cadr a)))))
                             (y2 (rational (py (aref v (cadr a)))))
                             (x3 (rational (px (aref v (car b)))))
                             (y3 (rational (py (aref v (car b)))))
                             (x4 (rational (px (aref v (cadr b)))))
                             (y4 (rational (py (aref v (cadr b)))))
                             (x2-x1 (- x2 x1))
                             (y2-y1 (- y2 y1))
                             (x4-x3 (- x4 x3))
                             (y4-y3 (- y4 y3))
                             (x1-x3 (- x1 x3))
                             (y1-y3 (- y1 y3))
                             (d (- (* x2-x1 y4-y3)
                                   (* x4-x3 y2-y1))))
                        (unless (zerop d)
                          (let ((r (/ (- (* x2-x1 y1-y3)
                                         (* y2-y1 x1-x3))
                                      d))
                                (s (/ (- (* x4-x3 y1-y3)
                                         (* y4-y3 x1-x3))
                                      d)))
                            (when (and (< 0 r 1)
                                       (< 0 s 1))
                              (break "lines intersect: ~s ~s~% ~s ~s - ~s ~s~% ~s ~s - ~s ~s~% ~s, ~s"
                                     a b
                                     (float x1 1d0) (float y1 1d0)
                                     (float x2 1d0) (float y2 1d0)
                                     (float x3 1d0) (float y3 1d0)
                                     (float x4 1d0) (float y4 1d0)
                                     (float r 1d0) (float s 1d0))
                              (setf ok nil)))))))
               (loop for j from (1+ i) below (length e)
                     do (check (aref e i) (aref e j)))))
    ok))

(defun voronoi (points)
  "calculate Voronoi diagram for vector POINTS of POINTs"
  (let ((state (make-voronoi-state points)))
    ;;
    (loop for e = (next-event state)
          while e
          do (etypecase e
               (circle-event
                (handle-circle state e))
               (point
                (add-site state e))))
    (finish-edges state)
    state))
