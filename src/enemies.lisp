(in-package :mako)

(ecs:define-component enemy)

(ecs:define-system move-enemies
    (:components-rw (tile position)
     :components-ro (enemy)
     :components-no (wait)
     :enable (not *player-turn*))
  (let ((dx (1- (random 3)))
        (dy (1- (random 3))))
    (assign-wait entity :remaining-time 60)
    (incf tile-col dx)
    (incf tile-row dy)
    (setf tile-hash (a*:encode-integer-coordinates tile-col tile-row)
          position-x (* tile-col +tile-size+)
          position-y (* tile-row +tile-size+))))
