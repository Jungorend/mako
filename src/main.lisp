(in-package #:mako)

(define-constant +repl-update-interval+ 0.3d0)

(defvar *resources-path*
  (asdf:system-relative-pathname :mako #P"Resources/"))

(deploy:define-hook (:boot set-resources-path) ()
  (setf *resources-path*
        (merge-pathnames #P"Resources/"
                         (uiop:pathname-parent-directory-pathname
                          (deploy:runtime-directory)))))

(defun init ()
  (ecs:make-storage)
  (setf *player-turn* t)
  (ecs:make-object
   `((:player)
     (:position :x 100.0 :y 100.0)
     (:stats)
     (:tile :col 3 :row 3)
     (:image :bitmap ,(load-bitmap "player.png")
             :width ,+tile-size+ :height ,+tile-size+)))
  (ecs:make-object
   `((:wall)
     (:position :x 320.0 :y 320.0)
     (:tile :col 10 :row 10)
     (:image :bitmap ,(load-bitmap "wall.png")
             :width ,+tile-size+ :height ,+tile-size+)))
  (ecs:make-object
   `((:enemy)
     (:position :x 200.0 :y 200.0)
     (:stats)
     (:tile :col 6 :row 6)
     (:image :bitmap ,(load-bitmap "enemy.png")
             :width ,+tile-size+ :height ,+tile-size+))))

(declaim (type fixnum *fps*))
(defvar *fps* 0)

(defun update (dt)
  (unless (zerop dt)
    (setf *fps* (round 1 dt)))
  (get-keyboard-input)
  (ecs:run-systems :dt (float dt 0.0)))

(defvar *font*)

(defun render ()
  (if (has-wait-p 0)
      (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 48 0
                    (format nil "Player Wait: ~d" (wait-remaining-time 0)))
      (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 48 0
                    (format nil "Player Wait: 0")))
  (when (has-wait-p 1)
    (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 72 0
                  (format nil "Enemy wait: ~d" (wait-remaining-time 1))))
  (al:draw-text *font* (al:map-rgba 255 255 255 0) 0 24 0
                (format nil "Position: ~d, ~d" (tile-col 0) (tile-row 0))))

(cffi:defcallback %main :int ((argc :int) (argv :pointer))
  (declare (ignore argc argv))
  (handler-bind
      ((error #'(lambda (e) (unless *debugger-hook*
                              (al:show-native-message-box
                               (cffi:null-pointer) "Hey guys"
                               "We got a big error here :("
                               (with-output-to-string (s)
                                 (uiop:print-condition-backtrace e :stream s))
                               (cffi:null-pointer) :error)))))
    (uiop:chdir (setf *default-pathname-defaults* *resources-path*))
    (al:set-app-name "mako")
    (unless (al:init)
      (error "Initializing liballegro failed"))
    (let ((config (al:load-config-file +config-path+)))
      (unless (cffi:null-pointer-p config)
        (al:merge-config-into (al:get-system-config) config)))
    (unless (al:init-primitives-addon)
      (error "Initializing primitives addon failed"))
    (unless (al:init-image-addon)
      (error "Initializing image addon failed"))
    (unless (al:init-font-addon)
      (error "Initializing liballegro font addon failed"))
    (unless (al:init-ttf-addon)
      (error "Initializing liballegro TTF addon failed"))
    (unless (al:install-audio)
      (error "Intializing audio addon failed"))
    (unless (al:init-acodec-addon)
      (error "Initializing audio codec addon failed"))
    (unless (al:restore-default-mixer)
      (error "Initializing default audio mixer failed"))
    (al:set-new-display-flags '(:opengl))
    (al:set-new-display-option :alpha-size 8 :require)
    (let ((display (al:create-display +window-width+ +window-height+))
          (event-queue (al:create-event-queue)))
      (when (cffi:null-pointer-p display)
        (error "Initializing display failed"))
      (al:inhibit-screensaver t)
      (al:set-window-title display "Mako")
      (al:register-event-source event-queue
                                (al:get-display-event-source display))
      (al:install-keyboard)
      (al:register-event-source event-queue
                                (al:get-keyboard-event-source))
      (al:install-mouse)
      (al:register-event-source event-queue
                                (al:get-mouse-event-source))
      (unwind-protect
           (cffi:with-foreign-object (event '(:union al:event))
             (init)
             (#+darwin trivial-main-thread:call-in-main-thread #-darwin funcall
              #'livesupport:setup-lisp-repl)
             (loop
               :named main-game-loop
               :with *font* := (al:ensure-loaded #'al:load-ttf-font
                                                 +font-path+
                                                 (- +font-size+) 0)
               :with ticks :of-type double-float := (al:get-time)
               :with last-repl-update :of-type double-float := ticks
               :with dt :of-type double-float := 0d0
               :while (loop
                        :named event-loop
                        :while (al:get-next-event event-queue event)
                        :for type := (cffi:foreign-slot-value
                                      event '(:union al:event) 'al::type)
                        :always (not (eq type :display-close)))
               :do (let ((new-ticks (al:get-time)))
                     (setf dt (- new-ticks ticks)
                           ticks new-ticks))
                   (when (> (- ticks last-repl-update)
                            +repl-update-interval+)
                     (livesupport:update-repl-link)
                     (setf last-repl-update ticks))
                   (al:clear-to-color (al:map-rgb 0 0 0))
                   (livesupport:continuable
                     (update dt)
                     (render))
                   (al:flip-display)
               :finally (al:destroy-font *font*)))
        (al:inhibit-screensaver nil)
        (al:destroy-event-queue event-queue)
        (al:destroy-display display)
        (al:stop-samples)
        (al:uninstall-system)
        (al:uninstall-audio)
        (al:shutdown-ttf-addon)
        (al:shutdown-font-addon)
        (al:shutdown-image-addon))))
  0)

(defun main ()
  (#+darwin trivial-main-thread:with-body-in-main-thread #-darwin progn nil
            (float-features:with-float-traps-masked
                (:divide-by-zero :invalid :inexact :overflow :underflow)
              (al:run-main 0 (cffi:null-pointer) (cffi:callback %main)))))
