(defsystem 3b-voronoi
  :description "voronoi graph generator"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria damn-fast-updatable-priority-queue)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "geometry")
               (:file "circle")
               (:file "edge")
               (:file "sweep-tree")
               (:file "voronoi")))
