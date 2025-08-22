(in-package :mako)

(ecs:define-component image
  "Contains ALLEGRO_BITMAP structure pointer, size, and scaling information."
  (bitmap (cffi:null-pointer) :type cffi:foreign-pointer)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (scale 1.0 :type single-float))

(declaim (inline load-bitmap))
(defun load-bitmap (filename)
  (al:ensure-loaded #'al:load-bitmap filename))

(ecs:define-system draw-images
    (:components-ro (position image)
     :initially (al:hold-bitmap-drawing t)
     :finally (al:hold-bitmap-drawing nil))
  (let ((scaled-width (* image-scale image-width))
        (scaled-height (* image-scale image-height)))
    (al:draw-scaled-bitmap image-bitmap 0 0
                           image-width image-height
                           (- position-x (* 0.5 scaled-width))
                           (- position-y (* 0.5 scaled-height))
                           scaled-width scaled-height 0)))

(defun make-sprite-object (x y image)
  (ecs:make-object
   `((:position :x x :y y)
     (:image :bitmap ,(load-bitmap image)
             :width 32.0 :height 32.0)
     (:tile :col (round/tile ,x)
            :row (round/tile ,y)))))
