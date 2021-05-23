#++
(ql:quickload '(alexandria 3b-glim/example/s 3b-voronoi))
(defpackage #:voronoi-vis-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:vertex #:textured #:solid
           #:position #:uv
           #:mv #:mvp #:lut #:tex #:line-base #:line-step #:debug1))
(defpackage #:voronoi-vis
  (:use #:cl #:voronoi-vis-shaders #:3b-glim-example/s)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:glim #:3b-glim/s)
                    (#:v #:3b-voronoi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shaders
(in-package #:voronoi-vis-shaders)
(input position :vec4 :location 0)
(input uv :vec4 :location 1)
(input color :vec4 :location 2)

(output color :vec4 :stage :fragment)

;; uniforms
(uniform mv :mat4) ;; model-view matrix
(uniform mvp :mat4) ;; model-view-projection matrix
(uniform tex :sampler-2d)
(uniform debug1 :int)

(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (position :vec4)
  (uv :vec4)
  (color :vec4))

;; generic vertex shader used for a few lighting models
(defun vertex ()
  (setf gl-position (* mvp position))
  (setf (@ outs position) (* mv position)
        (@ outs uv) uv
        (@ outs color) color))

(defun textured ()
  (let* ((uv (@ ins uv)))
    (setf color
          (vec4 (vec3
                 (* 0.5 (1+ (.x (texture tex (.xy uv))))))
                1))))

(defun solid ()
  (setf color (@ ins color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code
(in-package #:voronoi-vis)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec4) ;; uv
     (2 :vec4) ;; color
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun color (r &optional (g 0.0) (b 0.0) (a 1.0))
  (glim:attrib-f 2 r g b a))

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))

(defclass voronoi-vis (scratchpad)
  ((points :accessor points :initform nil)
   (state :accessor state :initform nil)
   (sweep :accessor sweep :initform 10000)
   (tex :accessor tex :initform nil))
  (:default-initargs :shaders '((:tex :vertex vertex :fragment textured)
                                (:solid :vertex vertex :fragment solid))))


(defvar *w* nil)
(defparameter *anim* nil)


(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview))))



(defun load-texture (w)
  (unless (tex w)
    (setf (tex w) (gl:gen-texture)))

  (gl:pixel-store :unpack-alignment 1)
  (gl:active-texture 0)
  (gl:bind-texture :texture-2d (tex w))

  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
  (let* ((w 512)
         (h 512)
         (a (make-array (list h w) :element-type 'single-float
                                   :initial-element 1.0)))
    (time
     (loop for j below h
           do (loop for i below w
                    do (setf (aref a j i)
                             (random 1.0)))))
    (gl:tex-image-2d :texture-2d 0 :r32f w h 0
                     :red :float (make-array (* w h)
                                             :element-type 'single-float
                                             :displaced-to a))))

(defparameter *click* :point)
#++
(setf (state *w*) nil)
#++
(length (points *w*))
#++(loop repeat 25 do (pop (points *w*)))
(defvar *f* ())
#++(setf (points *w*) (nth 0 *f*))

(defun nv (v &optional (l 1))
  (let* ((x (aref v 0))
         (y (aref v 1))
         (l (/ l (sqrt (+ (expt x 2) (expt y 2))))))
    (v::p (coerce (* x l) 'single-float)
          (coerce (* y l) 'single-float))))
#++
(push (loop for i in (points *w*)
            collect (v::p (v::px i) (v::py i)))
      *f*)

(defmethod display ((w voronoi-vis) now)
  (setf *w* w)
  (when *anim*
    (when (eql *anim* :step)
      (setf *anim* nil))
    (loop with d = 0.15
          with m = 768
          with n = 0                    ;(/ now 12.4)
          for a from 0
          for i in (points w)
          do (incf (aref i 0) (* d (sin (+ n (* 3 a)))))
             (incf (aref i 1) (* d (cos (+ n (* 5 a)))))
             (setf (aref i 0)
                   (mod (+ (aref i 0) m) m))
             (setf (aref i 1)
                   (mod (+ (aref i 1) m) m)))
    (setf (state *w*)
          (v::voronoi (points w)))
    #++(unless (v::check-edges (state w))
         (setf *anim* nil)))
  (glim:with-state (*format*)
    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :multisample :texture-2d)
    #++
    (when (tex w)
      (gl:active-texture 0)
      (gl:enable :texture-2d)

      (gl:bind-texture :texture-2d (tex w))

      (glim:uniform 'tex 0)

      (glim:uniform 'debug1 (if (aref *flags* 1) 1 0))

      (glim:with-pushed-matrix (:modelview)
        (glim:scale 1 1 1)
        (glim:scale 1.8 1.8 1)
        (glim:translate -0.5 -0.5 0)
        (gl:line-width 10)
        (uniforms)

        (glim:with-draw (:quads :shader :tex)

          (uv 0 1 0 0)
          (vertex 0 0)

          (uv 1 1 0 0)
          (vertex 1 0)

          (uv 1 0 0 0)
          (vertex 1 1)

          (uv 0 0 0 0)
          (vertex 0 1))
        (dispatch-draws w)))

    (gl:line-width 1)
    (glim:translate -1 -1 0)
    (glim:scale (/ 2 (wx w)) (/ 2 (wy w)) 1)

    (let* ((bx 768)
           (by 768)
           (dx 64)
           (dy 64)
           (mx (- (mx w) dx))
           (my (- (wy w) (my w) dy)))
      (glim:translate (+ dx 0.5) (+ dy 0.5) 0)
      (loop for i below 4
            when (aref *flags* i)
              do #++(glim:scale 0.5 0.5 1)
                 (glim:scale (/ (+ i 3)) (/ (+ 3 i)) 1))
      (uniforms)
      (glim:with-draw (:lines :shader :solid)
        (color 1 0 0 1)
        (vertex -64 0)
        (vertex (* bx 2) 0)
        (color 0 1 0 1)
        (vertex 0 -64)
        (vertex 0 (* by 2))
        (color 0.25 0.25 0.25 1)
        (vertex bx 0)
        (vertex bx by)
        (vertex bx by)
        (vertex 0 by)
        (color (random 1.0) (random 1.0) (random 1.0))
        (vertex (- mx 8) my)
        (vertex (+ mx 8) my)
        (vertex mx (- my 8))
        (vertex mx (+ my 8)))
      (when (points w)
        (color 1 0 1 1)
        (dispatch-draws w)
        (gl:point-size 1.4)
        ;; draw all points
        (glim:with-draw (:points :shader :solid)
          (loop for p in (points w)
                do (vertex (aref p 0) (aref p 1))))

        (when (state w)
          (let ((state (state w))
                (sweep (sweep w)))
            (glim:with-draw (:lines :shader :solid)
              (color 0.1 0.4 1.0 1)
              (vertex 0 sweep)
              (vertex bx sweep))
            ;; draw unseen sites
            (color 1 1 1 1)
            (dispatch-draws w)
            (gl:point-size 4)
            (glim:with-draw (:points :shader :solid)
              (loop for p in (v::sites state)
                    do (vertex (aref p 0) (aref p 1))))
            ;; draw active sites
            (color 0 1 0 1)
            (dispatch-draws w)
            (gl:point-size 6)
            (glim:with-draw (:points :shader :solid)
              (loop for e = (v::table-start (v::beachline state))
                      then (v::next e)
                    for p = (when e (v::right-focus e))
                    while p
                    do (vertex (aref p 0) (aref p 1))))
            #++
            (glim:with-draw (:points :shader :solid)
              (color (random 1.0) (random 1.0) (random 1.0) 1)
              (vertex 511 607 0)
              (vertex 572.5 591.5 0)
              (vertex 510.268352365416 607.8066884176183 0))
            ;; draw active arcs and edges
            (glim:with-draw (:lines :shader :solid)
              (loop with drawn = (make-hash-table)
                    for e = (v::table-start (v::beachline (state *w*)))
                      then (v::next e)
                    for p = (when e (v::right-focus e))
                    for q = (when p (v::quadratic p sweep))
                    for i from 0 by 2
                    while p
                    do (flet ((e (x)
                                (if (numberp q)
                                    q
                                    (let ((x2 (* x x)))
                                      (+ (* (aref q 0) x2)
                                         (* (aref q 1) x)
                                         (aref q 2))))))
                         (if (and (v::left-focus e) (v::right-focus e))
                             ;; edges
                             (let ((x (v::intersect-edge e sweep)))
                               (cond
                                 ((minusp (v::edge-type e))
                                  (color 1 1 1 1)
                                  (vertex (- x 0) (+ sweep i))
                                  (vertex (- x 4) (+ sweep 12 i)))
                                 ((plusp (v::edge-type e))
                                  (color 0 1 1 1)
                                  (vertex x (- sweep i))
                                  (vertex (+ x 4) (- sweep 12 i)))
                                 (t
                                  (color (random 1.0) (random 1.0))
                                  (vertex x (- sweep 8))
                                  (vertex x (+ sweep 8))))
                               (when (v::dir e)
                                 (let ((d (nv (v::dir e) 32))
                                       (y (e x)))
                                   (color 1 0.7 0.2 1)
                                   (vertex x y)
                                   (vertex (+ x (aref d 0))
                                           (+ y (aref d 1))))))
                             ;; clamp one-sided edges to box
                             (let ((x (if (v::left-focus e) bx 0)))
                               (cond
                                 ((minusp (v::edge-type e))
                                  (color 1 1 1 1)
                                  (vertex (- x 0) (+ sweep i))
                                  (vertex (- x 4) (+ sweep 12 i)))
                                 ((plusp (v::edge-type e))
                                  (color 0 1 1 1)
                                  (vertex x (- sweep i))
                                  (vertex (+ x 4) (- sweep 12 i)))
                                 (t
                                  (color (random 1.0) (random 1.0))
                                  (vertex x (- sweep 8))
                                  (vertex x (+ sweep 8))))))
                         (color 0 1 0 1)
                         (cond
                           ;; degenerate arcs
                           ((= sweep (aref p 1))
                            (vertex (aref p 0) (aref p 1))
                            (vertex (aref p 0) by))
                           ;; normal arcs
                           ((< sweep (aref p 1))
                            ;; draw whole parabola once in gray
                            (unless (gethash p drawn)
                              (color 0.2 0.2 0.2 1)
                              (setf (gethash p drawn) t)
                              (loop for pp = nil then (list x y)
                                    for x below bx by 8
                                    for y = (e x)
                                    when pp
                                      do (apply #'vertex pp)
                                         (vertex x y)))
                            ;; then draw active part
                            (when (and (v::next e)
                                       (v::left-focus e)
                                       (v::right-focus e)
                                       (v::left-focus (v::next e))
                                       (v::right-focus (v::next e)))
                              (let ((l (v::intersect-edge e sweep))
                                    (r (v::intersect-edge (v::next e) sweep)))
                                (let ((r 1)
                                      (g (if (v::right-circle e) 1 0))
                                      (b (if (v::left-circle (v::next e))
                                             1 0)))
                                  (color r g b 1))
                                (loop for pp = nil then (list x y -0.1)
                                      for x from l upto r by 5
                                      for y = (e x)
                                      when pp
                                        do (apply #'vertex pp)
                                           (vertex x y -0.1))))))))
              ;; draw circle events
              (color 0.3 0.3 0.3 1)
              (damn-fast-updatable-priority-queue:do-queue
                  (e (v::q (state w)))
                (let ((cx (v::px (v::c e)))
                      (cy (v::py (v::c e)))
                      (r (v::r e))
                      (d 0.0625))
                  (loop for a upto (* 2 pi) by d
                        do (vertex (+ cx (* r (sin a)))
                                   (+ cy (* r (cos a))))
                           (vertex (+ cx (* r (sin (+ a d))))
                                   (+ cy (* r (cos (+ a d)))))))))
            (dispatch-draws w)
            ;; draw voronoi graph so far
            (gl:point-size 5)
            #++
            (glim:with-draw (:points :shader :solid)
              (color (random 1.0) (random 1.0) (random 1.0) 1)
              (loop for i across (v::vertices state)
                    do (vertex (v::px i) (v::py i) 0)))
            (gl:line-width 0.75)
            (color 1 0.25 0.5 1)

            (glim:with-draw (:lines :shader :solid)
              (loop for (i j) across (v::edges state)
                    for a = (aref (v::vertices state) i)
                    for b = (aref (v::vertices state) j)
                    do (vertex (v::px a) (v::py a) 0)
                       (vertex (v::px b) (v::py b) 0)))

            (when (v::cross-edges (state w))
              (dispatch-draws w)
              (gl:line-width 4)
              (color 1 0 1 1)
              (glim:with-draw (:lines :shader :solid)
                (color (random 1.0) (random 1.0) (random 1.0) 1)
                (loop for e in (v::cross-edges state)
                      for (i j) = (aref (v::edges state) e)
                      for a = (aref (v::vertices state) i)
                      for b = (aref (v::vertices state) j)
                      do (vertex (v::px a) (v::py a) 0)
                         (vertex (v::px b) (v::py b) 0))))
            (dispatch-draws w)))
        (when (aref *flags* 9)
          (let ((*random-state* (sb-ext:seed-random-state 1)))
            (glim:with-draw (:lines :shader :solid)
              (color 1 0 0.5 1)
              (vertex 0 my)
              (vertex bx my))
            (dispatch-draws w)
            (gl:point-size 1.5)
            (loop
              for p in (points w)
              do (color (random 1.0) (random 1.0) (random 1.0) 1)
                 (when (and (< my (aref p 1))
                            (or (aref *flags* 8)
                                (<= (sweep w) (aref p 1))))

                   (let ((q (v::quadratic p my)))
                     (flet ((e (x)
                              (let ((x2 (* x x)))
                                (+ (* (aref q 0) x2)
                                   (* (aref q 1) x)
                                   (aref q 2)))))
                       (glim:with-draw (:line-strip :shader :solid)
                         (loop for x below bx by 8
                               for y = (e x)
                               do (vertex x y)))))))))))
    (dispatch-draws w)))

(defmethod mouse ((window voronoi-vis) button state x y)
  #++(format t "click ~s ~s~%" button state)
  (let* ((bx 768)
         (by 768)
         (dx 64)
         (dy 64)
         (mx (- (mx window) dx))
         (my (- (wy window) (my window) dy)))
    (when (and (eql button :left-button)
               (eql state :down)
                                        ;(eql *click* :point)
               (<= 0 mx bx)
               (<= 0 my by))
      (push (make-array 2 :element-type 'single-float
                          :initial-contents (list (coerce mx 'single-float)
                                                  (coerce my 'single-float)))
            (points window))
      (format t "~s points~%" (length (points window))))))

(defun reset (w &key points)
  (when points
    (setf (points w) nil))
  (setf (state w) nil
        (sweep w) 1234567))

(defun start (w)
  (reset w)
  (when (and (points w) (cdr (points w)))
    (setf (state w) (v::make-voronoi-state (points w))
          (sweep w) (+ 10 (reduce 'max (points w) :key 'v::py)))))

#++
(setf (state *w*) nil)
#++
(incf (sweep *w*) -1)
#++
(v::dump-tree (v::beachline (state *w*)) :y (sweep *w*))
#++
(v::dump-tree (v::beachline (state *w*)) :y 199.86896)
#++
(loop for i in (v::sites (state *w*))
      do (format t "~s ~s ~%" (v::px i) (- 768 (v::py i))))
#++
(v::peek-next-event (state *w*))
#(746.0332 199.86896)

(defun p (x y)
  (make-array 2 :element-type 'single-float
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))

#++
(setf (points *w*) (list (p 128 600) (p 512 600) (p 200 500) (p 550 400)))
#++
(setf (points *w*) (list (p 128 550) (p 512 600) (p 200 500) (p 550 400)))

(defun next-event (window &key dump)
  (let ((e (v::next-event (state window))))
    (when dump
      (format t "~&~%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%")
      (format t "  next event = ~s~%" e))

    (etypecase e
      (null ;; done
       (v::finish-edges (state window)))
      (v::circle-event
       (when dump
         (format t " ~s  @ ~s~%" (v::r e) (v::pe e)))
       (setf (sweep window) (v::py (v::pe e)))
       (v::handle-circle (state window) e))
      (v::point
       (setf (sweep window) (v::py e))
       (v::add-site (state window) e)))))

(defmethod keyboard ((window voronoi-vis) key x y)
  (declare (ignore x y))

  (case key
    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\d (setf v::*dump* (not v::*dump*)))
    (#\a (if *anim*
             (setf *anim* nil)
             (setf *anim* 128)))
    (#\s
     (setf *anim* :step))
    (#\c
     (setf *anim* nil)
     (reset window :points t))
    (#\r
     (reset window :points nil))
    (#\+
     (loop repeat 10
           do (push (v::p (random 768.0) (random 768.0))
                    (points window)))
     (format t "points = ~s~%" (length (points window))))
    (#\n
     (if (state window)
         (let* ((e (v::peek-next-event (state window)))
                (y (typecase e
                     (v::circle-event
                      (v::py (v::pe e)))
                     (v::point
                      (v::py e)))))
           (if (and y (> y (1- (sweep window))))
               (next-event window)
               (decf (sweep window))))
         (start window)))
    (#\g
     (unless (state window)
       (start window))
     (time
      (progn
        (loop while (v::peek-next-event (state window))
              do (next-event window))
        (v::finish-edges (state window)))))
    (#\space
     (if (state window)
         (next-event window :dump t)
         (start window)))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod init-gl ((w voronoi-vis))
  (gl:pixel-store :unpack-alignment 1)

  (gl:disable :dither))

(defun voronoi-vis (&rest args)
  (glut:display-window (apply #'make-instance 'voronoi-vis args)))

#++
(ql:quickload 'voronoi-vis)
#++
(voronoi-vis :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)
