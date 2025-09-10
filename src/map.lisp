(in-package :mako)

(defvar *map-size* 10)
(defvar *room-size* 5)
(defvar *room-size-variance* 2)
(defvar *room-attempt-count* 10)

(defun build-room (map)
  (let ((origin-x (random *map-size*))
        (origin-y (random *map-size*))
        (room-size-x (+ *room-size* (- (random (* 2 *room-size-variance*))
                                       *room-size-variance*)))
        (room-size-y (+ *room-size* (- (random (* 2 *room-size-variance*))
                                       *room-size-variance*))))
    (loop for x below room-size-x do
          (loop for y below room-size-y do
                (let ((row (+ y origin-y))
                      (col (+ x origin-x)))
                  (if (and (>= row 1)
                           (< row (1- *map-size*))
                           (>= col 1)
                           (< col (1- *map-size*)))
                      (setf (aref map col row) nil)))))))

(defun build-level ()
  (let ((map (make-array `(,*map-size* ,*map-size*) :element-type 'symbol :initial-element 'wall)))
    (build-room map)
    map))

(defun load-level (map)
  (loop for col below (array-dimension map 0) do
        (loop for row below (array-dimension map 1) do
              (case (aref map col row)
                (wall (spawn-wall col row))
                (enemy (spawn-enemy col row))
                (nil nil)))))
