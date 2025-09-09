(in-package :mako)

(ecs:define-component wall)

(defun spawn-wall (x y &key (image "wall.png"))
  (ecs:make-object
   `((:wall)
     (:position :x ,(* x +tile-size+) :y ,(* y +tile-size+))
     (:tile :col ,x :row ,y)
     (:image :bitmap ,(load-bitmap image)
             :width +tile-size+ :height +tile-size+))))
