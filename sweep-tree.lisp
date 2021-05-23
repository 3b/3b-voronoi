(in-package #:3b-voronoi)

(defclass sweep-table-node (sweep-edge)
  (;; parent
   (tree-up :accessor tree-up :initarg :tree-up :initform nil)
   ;; children
   (tree-left :accessor tree-left :initform nil :initarg :tree-left)
   (tree-right :accessor tree-right :initform nil :initarg :tree-right)
   ;; in-order siblings
   (prev :accessor prev :initform nil :initarg :prev)
   (next :accessor next :initform nil :initarg :next)))

(defclass sweep-table ()
  ((root :accessor root :initform nil :initarg :root)))

(defmethod initialize-instance :after ((o sweep-table) &key)
  (when (root o)
    (setf (tree-up (root o)) o)))


(defun search-table (table x y)
  (if (null (root table))
      (values nil nil)
      (loop for node = (root table) then next
            for before = (before-edge-p node x y)
            for next = (if before (tree-left node) (tree-right node))
            while next
            finally (return (if before
                                (values (prev node) node)
                                (values node (next node)))))))
(defun table-start (table)
  ;;return leftmost edge, mostly for debugging
  (let ((r (root table)))
    ;; searching by LEFT and PREV should be equivalent, but LEFT
    ;; should be logn if tree is balanced
    (loop while (and r (tree-left r))
          do (setf r (tree-left r)))
    r))

(defun insert-between (edge a b)
  (when a (assert (eql b (next a))))
  (when b (assert (eql a (prev b))))
  (assert (or a b))
  ;; not sure if callers should be passing a sweep-table-node or edge?
  (when (typep edge 'sweep-edge)
    ;; if passed a sweep edge, turn it into a sweep-table-node
    (change-class edge 'sweep-table-node))
  ;; make sure node isn't in tree yet
  (assert (null (tree-up edge)))
  (assert (null (tree-left edge)))
  (assert (null (tree-right edge)))
  (flet ((insert-before (x)
           (setf (tree-up edge) x)
           (setf (tree-left edge) (tree-left x))
           (when (tree-left x)
             (setf (tree-up (tree-left x)) edge))
           (setf (tree-left x) edge))
         (insert-after (x)
           (setf (tree-up edge) x)
           (setf (tree-right edge) (tree-right x))
           (when (tree-right x)
             (setf (tree-up (tree-right x)) edge))
           (setf (tree-right x) edge)))
    (setf (next edge) b)
    (setf (prev edge) a)
    (when a (setf (next a) edge))
    (when b (setf (prev b) edge))
    (cond
      ((not a) (insert-before b))
      ((not b) (insert-after a))
      (t (if (zerop (random 2))
             (insert-after a)
             (insert-before b))))))

(defun insert-before (edge node)
  (insert-between edge (prev node) node))

(defun insert-after (edge node)
  (insert-between edge node (next node)))

(defun delete-node (node)
  (labels ((replace-node (node &optional (new nil))
             (let ((up (tree-up node)))
               (etypecase up
                 (sweep-table
                  (assert (eql node (root up)))
                  (setf (root up) new))
                 (sweep-table-node
                  (cond
                    ((eql node (tree-left up))
                     (setf (tree-left up) new))
                    ((eql node (tree-right up))
                     (setf (tree-right up) new))
                    (t
                     (error "deleted node ~s that wasn't child of parent ~s?"
                            node up)))))
               (when new
                 (setf (tree-up new) up))
               (when (next node)
                 (setf (prev (next node)) (prev node)))
               (when (prev node)
                 (setf (next (prev node)) (next node))))))
    (cond
      ;; leaf node
      ((and (not (tree-left node)) (not (tree-right node)))
       (replace-node node nil))
      ;; only 1 child, promote it
      ((or (not (tree-left node)) (not (tree-right node)))
       (replace-node node (or (tree-left node) (tree-right node))))
      ;; 2 children, move one child up, then if needed,
      ;; move conflicting grandchild to other child. If
      ;; NODE has 2 children and we are promoting left
      ;; child (symmetrical for promoting right child): if
      ;; LEFT has no RIGHT child, it can replace NODE
      ;; directly. If LEFT has a RIGHT child, it can be
      ;; moved to LEFT child of (NEXT NODE). (if NODE has 2
      ;; children, NEXT must be a descendant of (RIGHT
      ;; NODE) and cannot have a LEFT child.)
      (t
       (let* ((dir (cond
                     ;; if exactly one of children lacks a
                     ;; child on side corresponding to node,
                     ;; promote that child
                     ((and (tree-right (tree-left node))
                           (not (tree-left (tree-right node))))
                      :right)
                     ((and (tree-left (tree-right node))
                           (not (tree-right (tree-left node))))
                      :left)
                     ;; otherwise pick a child at random
                     (t (if (zerop (random 2)) :left :right))))
              ;; child to promote
              (child (if (eql dir :left)
                         (tree-left node)
                         (tree-right node)))
              ;; grandchild to move if needed
              (move (if (eql dir :left)
                        (tree-right child)
                        (tree-left child)))
              (move-to (if (eql dir :left)
                           (next node)
                           (prev node))))
         (if (eql dir :left)
             (assert (not (tree-left move-to)))
             (assert (not (tree-right move-to))))
         (when move
           (if (eql dir :left)
               (setf (tree-left move-to) move
                     (tree-right child) nil)
               (setf (tree-right move-to) move
                     (tree-left child) nil))
           (setf (tree-up move) move-to))
         (replace-node node child)
         (if (eql dir :left)
             (setf (tree-right child) (tree-right node)
                   (tree-up (tree-right node)) child)
             (setf (tree-left child) (tree-left node)
                   (tree-up (tree-left node)) child))))))
  (setf (tree-up node) :deleted))

;; for testing tree ops, version of search that just uses a hash of
;; edge positions instead of calculating based on sweep lines
(defun %test-search-table (table x edge-x)
  (flet ((before-edge-p (node x)
           (cond
             ;; at edge of tree, left or right is NIL, so we always go other
             ;; direction from that
             ((not (right-focus node))
              t)
             ((not (left-focus node))
              nil)
             (t (< x (gethash node edge-x))))))
    (if (null (root table))
        (values nil nil)
        (loop for node = (root table) then next
              for before = (before-edge-p node x)
              for next = (if before (tree-left node) (tree-right node))
              while next
              finally (return (if before
                                  (values (prev node) node)
                                  (values node (next node))))))))

  ;; verify tree invariants of tree built with %test-search-table
(defun %test-check-tree (root edge-x)
  (let ((start nil)
        (end nil)
        (on-tree (make-hash-table)))
    (labels ((x (n)
               (gethash n edge-x))
             (r (node)
               (when node
                 (assert (not (gethash node on-tree)))
                 (setf (gethash node on-tree) t)
                 (when (not (prev node))
                   (assert (not start))
                   (setf start node))
                 (when (not (next node))
                   (assert (not end))
                   (setf end node))
                 (when (tree-left node)
                   (assert (eql node (tree-up (tree-left node))))
                   (assert (<= (x (tree-left node))
                               (x node)))
                   (r (tree-left node)))
                 (when (tree-right node)
                   (assert (eql node (tree-up (tree-right node))))
                   (assert (<= (x node) (x (tree-right node))))
                   (r (tree-right node))))))
      (when (root root)
        (r (root root))
        (assert (and start end))
        (assert (not (prev start)))
        (assert (not (next end)))
        (loop for n = start then (next n)
              do (if (next n)
                     (assert (eql (prev (next n)) n))
                     (assert (eql n end)))
                 (assert (gethash n on-tree))
              while (next n))))))

(defun %dump-tree (st edge-x)
  (labels ((dump-node (n indent)
             (when (tree-left n)
               (dump-node (tree-left n) (concatenate 'string "  " indent))
               (assert (eql (tree-up (tree-left n)) n)))
             (format t "~&~a ~s @ ~,3f~%" indent n (gethash n edge-x))
             (when (tree-right n)
               (dump-node (tree-right n) (concatenate 'string "  " indent))
               (assert (eql (tree-up (tree-right n)) n)))))
    (when (and st (root st))
      (format t "~& ---- dump tree~%")
      (dump-node (root st) ""))))
