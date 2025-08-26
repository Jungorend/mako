(in-package :mako)

(ecs:define-component wall)

(defun create-wall (x y &key (image "steel-wall.png"))
  (ecs:make-object
   `((:wall)
     (:position :x ,(* x +tile-size+) :y ,(* y +tile-size+))
     (:tile :col ,y :row ,x)
     (:image :bitmap ,(load-bitmap image)
             :width +tile-size+ :height +tile-size+))))
