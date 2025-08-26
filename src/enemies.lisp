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
    (incf tile-row dx)
    (incf tile-col dy)
    (setf tile-hash (a*:encode-integer-coordinates tile-col tile-row)
          position-x (* tile-row +tile-size+)
          position-y (* tile-col +tile-size+))))
