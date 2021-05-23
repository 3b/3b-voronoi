(in-package #:3b-voronoi)



(defclass circle-event ()
  ;; POINT of event location (bottom of circle through p1,p2,p3)
  ((pe :reader pe :initarg :pe)
   ;; center POINT of circle
   (c :reader c :initarg :c)
   ;; radius (mostly for drawing it for debugging)
   (r :reader r :initarg :r)
   ;; edge nodes of edges bounding arc to be removed
   (left-edge :reader left-edge :initarg :left-edge)
   (right-edge :reader right-edge :initarg :right-edge)))
