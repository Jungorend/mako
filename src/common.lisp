(in-package #:mako)

(define-constant +window-width+ 1280)
(define-constant +window-height+ 800)
(define-constant +tile-size+ 32.0)
(define-constant +ui-font-size+ 28)
(define-constant +font-path+ "inconsolata.ttf" :test #'string=)
(define-constant +font-size+ 24)
(define-constant +config-path+ "../config.cfg" :test #'string=)

(defclass action-state ()
  ((binding :reader binding :initarg :binding)
   (pressed? :accessor pressed? :initform nil :type boolean)
   (down? :accessor down? :initform nil :type boolean)))

(defvar *keyboard-state* (make-hash-table))
(setf
 (gethash 'move-up *keyboard-state*) (make-instance 'action-state :binding :w)
 (gethash 'move-down *keyboard-state*) (make-instance 'action-state :binding :s)
 (gethash 'move-left *keyboard-state*) (make-instance 'action-state :binding :a)
 (gethash 'move-right *keyboard-state*) (make-instance 'action-state :binding :d))

(defun key-pressed? (action)
  (pressed? (gethash action *keyboard-state*)))
(defun set-key-pressed? (action state)
  (setf (pressed? (gethash action *keyboard-state*)) state))
(defsetf key-pressed? set-key-pressed?)

(defun key-down? (action)
  (down? (gethash action *keyboard-state*)))
(defun set-key-down? (action state)
  (setf (down? (gethash action *keyboard-state*)) state))
(defsetf key-down? set-key-down?)

(defun get-keyboard-input ()
  (al:with-current-keyboard-state keyboard-state
    (loop for bind being the hash-key using (hash-value action) of *keyboard-state* do
      (if (al:key-down keyboard-state (binding action))
          (if (down? action)
              (when (pressed? action)
                (setf (pressed? action) nil))
              (setf (pressed? action) t
                    (down? action) t))
          (when (down? action)
            (setf (pressed? action) nil
                  (down? action) nil))))))

(declaim (type boolean *player-turn* *paused*))
(defvar *player-turn* t)
(defvar *paused* nil)

(declaim (inline round/tile-size)
         (ftype (function (single-float) fixnum) round/tile))
(defun round/tile (x)
  (round x +tile-size+))

(ecs:define-component tile
    (col 0 :type fixnum)
  (row 0 :type fixnum)
  (hash (a*:encode-integer-coordinates col row)
        :type fixnum :index tiles))

(ecs:define-component position
    (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(ecs:define-component wait
    (remaining-time 10 :type fixnum))

(ecs:define-system advance-game-clock
    (:components-rw (wait)
     :enable (or (not *player-turn*)
                 (not *paused*)))
  (decf wait-remaining-time 1)
  (when (= wait-remaining-time 0)
    (delete-wait entity)))
