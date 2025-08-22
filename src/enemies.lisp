(in-package :mako)

(ecs:defcomponent enemy)

(ecs:defsystem move-enemies
  (:components-rw (tile position)
   :components-ro (enemy)
   :enable (not *player-turn*)
   :finally (setf *player-turn* t))
  (let ((dx (1- (random 3)))
        (dy (1- (random 3))))
    (incf position-x (* dx +tile-size+))
    (incf position-y (* dy +tile-size+))
    (setf tile-row (round/tile position-x)
          tile-col (round/tile position-y)
          tile-hash (a*:encode-integer-coordinates tile-col tile-row))))
