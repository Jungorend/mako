(in-package :mako)

(ecs:defcomponent player)

(ecs:define-system move-player
    (:components-ro (player)
     :components-rw (position tile)
     :enable (and *player-turn*
                  (not *paused*)))
  (let ((dx 0.0) (dy 0.0))
    (when (key-pressed? 'move-up) (decf dy 1.0))
    (when (key-pressed? 'move-down) (incf dy 1.0))
    (when (key-pressed? 'move-left) (decf dx 1.0))
    (when (key-pressed? 'move-right) (incf dx 1.0))
    (when (or (not (= dx 0.0))
              (not (= dy 0.0)))
      (incf position-x (* dx +tile-size+))
      (incf position-y (* dy +tile-size+))
      (setf tile-row (round/tile position-x)
            tile-col (round/tile position-y)
            tile-hash (a*:encode-integer-coordinates tile-col tile-row))
      (setf *player-turn* nil))))
