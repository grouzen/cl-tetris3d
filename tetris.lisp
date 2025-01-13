;;;; Clone of the original game "Tetris".
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(in-package :cl-tetris)

(defun gl-init (width height)
  (progn
    (gl:clear-color 0.1 0.1 0.1 0)
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 (+ width 100) 0 (+ height 100) -1 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defun random-color ()
  (let ((color (random 6))
        (colors (list :pink :thistle :rosy-brown :powder-blue :dark-sea-green :salmon)))
    (nth color colors)))

(defmacro do-dotimes-twice ((h w) &body body)
  `(dotimes (h ,h)
     (dotimes (w ,w)
       ,@body)))

(defun build-square (height width)
  (let ((h (/ height 2))
        (w (/ width 2)))
    (gl:push-matrix)
    (gl:with-primitives :quads
      (gl:vertex (* -1 w) (* -1 h))
      (gl:vertex (* 1 w) (* -1 h))
      (gl:vertex (* 1 w) (* 1 h))
      (gl:vertex (* -1 w) (* 1 h)))
    (gl:pop-matrix)))

(defclass figure ()
  ((x :initarg :x :accessor x :initform 0)
   (y :initarg :y :accessor y :initform 0)
   (x-d :initarg :x-d :accessor x-d :initform 0)
   (color :initarg :color :accessor color :initform (random-color))
   (body :initarg :body :accessor body :initform nil)))

(defclass arena ()
  ((width :initarg :width :accessor width :initform 12)
   (height :initarg :height :accessor height :initform 18)
   (field :initarg :field 
          :initform (make-array '(18 12) :initial-element nil)
          :accessor field)))

(defparameter *figures*
  (list
   (make-array '(2 3) :initial-contents '((1 1 1) (1 0 0)))
   (make-array '(2 3) :initial-contents '((1 1 1) (0 0 1)))
   (make-array '(2 3) :initial-contents '((1 1 0) (0 1 1)))
   (make-array '(2 3) :initial-contents '((0 1 1) (1 1 0)))
   (make-array '(2 2) :initial-contents '((1 1) (1 1)))
   (make-array '(1 4) :initial-contents '((1 1 1 1)))
   (make-array '(2 3) :initial-contents '((1 1 1) (0 1 0)))))

(defgeneric rotate-figure-clockwise (figure)
  (:method ((figure figure))
    (with-slots (body) figure
      (let* ((h (array-dimension body 0))
             (w (array-dimension body 1))
             (new-body (make-array (list w h))))
        (dotimes (i h)
          (dotimes (j w)
            (setf (aref new-body j (- h i 1)) (aref body i j))))
        (setf body new-body)))))

(defgeneric draw-arena (arena)
  (:method ((arena arena))
    (with-slots (width height field) arena
      (gl:push-matrix)
      (gl:translate 40 40 0)
      (do-dotimes-twice (height width)
        (gl:push-matrix)
        (gl:translate (* w 60) (* h 60) 0)
        (let ((cell (aref field h w)))
          (when (not (null cell))
            (gl:polygon-mode :front-and-back :fill)
            (case cell
              (:pink (gl:color 1 0.75 0.8))
              (:thistle (gl:color 0.85 0.75 0.85))
              (:rosy-brown (gl:color 0.74 0.56 0.56))
              (:powder-blue (gl:color 0.69 0.88 0.9))
              (:dark-sea-green (gl:color 0.56 0.74 0.56))
              (:salmon (gl:color 1 0.63 0.48)))
            (build-square 50 50)))
        (gl:color 0.3 0.3 0.3)
        (gl:polygon-mode :front-and-back :line)
        (build-square 50 50)
        (gl:pop-matrix))
      (gl:pop-matrix))))

(defgeneric vanish-lines (arena)
  (:method ((arena arena))
    (flet ((fill-line (new-h arena-h new-field arena-field)
             (dotimes (w (array-dimension new-field 1))
               (setf (aref new-field new-h w) (aref arena-field arena-h w)))))
      (let ((score 0))
        (with-slots (width height field) arena
          (setf field
                (let ((new-field (make-array `(,height ,width) :initial-element nil))
                      (new-h 0))
                  (dotimes (h height)
                    (let ((vanish-p t))
                      (dotimes (w width)
                        (when (null (aref field h w))
                          (setf vanish-p nil)))
                      (if vanish-p
                          (incf score)
                          (progn
                            (fill-line new-h h new-field field)
                            (incf new-h)))))
                  new-field)))
        score))))

(defgeneric figure->arena (figure arena)
  (:method ((figure figure) (arena arena))
    (with-slots (x y body color) figure
      (with-slots (field) arena
        (do-dotimes-twice ((array-dimension body 0) (array-dimension body 1))
          (when (= (aref body h w) 1)
            (setf (aref field (+ y h) (+ x w)) color)))))))

(defgeneric draw-world (arena figure)
  (:method ((arena arena) (figure figure))
    (gl:clear :color-buffer-bit)
    ;;(gl:translate (* -1 (/ width 2)) (* -1 (/ height 2)) z)
    (gl:load-identity)
    (draw-arena arena)
    (draw-figure figure arena)
    (gl:flush)))

(defgeneric draw-figure (figure arena)
  (:method ((figure figure) (arena arena))
    (with-slots (x y color body) figure
      (case color
        (:pink (gl:color 1 0.75 0.8))
        (:thistle (gl:color 0.85 0.75 0.85))
        (:rosy-brown (gl:color 0.74 0.56 0.56))
        (:powder-blue (gl:color 0.69 0.88 0.9))
        (:dark-sea-green (gl:color 0.56 0.74 0.56))
        (:salmon (gl:color 1 0.63 0.48)))
      (gl:polygon-mode :front-and-back :fill)
      (do-dotimes-twice ((array-dimension body 0) (array-dimension body 1))
        (when (= (aref body h w) 1)
          (gl:push-matrix)
          (gl:translate (+ 40 (* w 60) (* x 60)) (+ (* h 60) (* y 60)) 0)
          (build-square 50 50)
          (gl:pop-matrix))))))

(defgeneric move-figure (figure arena &key move-sideways)
  (:method ((figure figure) (arena arena) &key move-sideways)
    (flet ((walls-collision-p (figure arena)
             (with-slots (x y x-d body) figure 
               (with-slots (width height field) arena
                 (let ((figure-h (array-dimension body 0))
                       (figure-w (array-dimension body 1))
                       (next-x (+ x x-d)))
                   (when (or (< next-x 0)
                             (> (+ next-x figure-w) width)
                             (block walls
                               (do-dotimes-twice (figure-h figure-w)
                                 (when (and (< (+ y h) height) (= (aref body h w) 1))
                                   (when (not (null (aref field (+ y h) (+ next-x w))))
                                     (return-from walls t))))))
                     t)))))
           (floor-collision-p (figure arena)
             (with-slots (x y body) figure
               (with-slots (width height field) arena
                 (let ((next-y (1- y))
                       (figure-h (array-dimension body 0))
                       (figure-w (array-dimension body 1)))
                   (if (< next-y 0)
                       t
                       (do-dotimes-twice (figure-h figure-w)
                         (when (and (< (+ next-y h) height) (= (aref body h w) 1))
                           (when (not (null (aref field (+ next-y h) (+ x w))))
                             (return-from floor-collision-p t))))))))))
      (with-slots (x y x-d body) figure
        (with-slots (width height field) arena
          (let ((terminate nil))
            (when (not (walls-collision-p figure arena))
              (setf x (+ x x-d)))
            (if (not move-sideways)
                (if (not (floor-collision-p figure arena))
                    (setf y (- y 1))
                    (setf terminate t)))
            (setf x-d 0)
            terminate))))))

(defgeneric rotate-collision-p (figure arena)
  (:method ((figure figure) (arena arena))
    (with-slots (width height field) arena
      (with-slots (x y body) figure
        (do-dotimes-twice ((array-dimension body 0) (array-dimension body 1))
          (when (and (= (aref body h w) 1) (< (+ y h) height))
            (when (or (< (+ x w) 0)
                      (>= (+ x w) width)
                      (< (+ y h) 0)
                      (not (null (aref field (+ y h) (+ x w)))))
              (return-from rotate-collision-p t))))))))

(defgeneric choose-figure (figure arena)
  (:method ((figure figure) (arena arena))
    (setf *random-state* (make-random-state t))
    (let ((choise (random 7)))
      (with-slots (x y) figure
        (with-slots (width height) arena
          (setf x (1- (floor (/ width 2)))
                y height)))
      (setf (body figure) (nth choise *figures*))
      figure)))

(defun run (&key (width 640) (height 1000) (bpp 32))
  (sdl:with-init ()
    (unless (sdl:window width height
                        :bpp bpp
                        :opengl t
                        :position :center
                        :title-caption "Werka's Tetris"
                        :opengl-attributes '((:sdl-gl-doublebuffer 1)))
      (error "Unable to create SDL window"))
    (setf (sdl:frame-rate) 40)
    (sdl:enable-key-repeat 50 50)
    (gl-init width height)

    (format t "Key Bindings:
       Left:  move left
       Right: move right
       Down:  land
       Up:    rotate
       Space: pause/unpause
       Esc:   quit~%
 Level | Speed | Score~%")
        (let* ((arena (make-instance 'arena
                                :width 12
                                :height 18
                                :field (make-array '(18 12) :initial-element nil)))
           (figure (choose-figure (make-instance 'figure) arena))
           (ticks (sdl:system-ticks))
           (run t)
           (score 0)
           (level-score 0)
           (hz 3)
           (level 0))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
                         (case key
                           (:SDL-KEY-ESCAPE (sdl:push-quit-event))
                           (:SDL-KEY-SPACE
                            (setf run (not run))
                            (format t "~%Game ~A!~%" (if run "unpaused" "paused")))
                           (:SDL-KEY-DOWN
                            (do () ((move-figure figure arena))))
                           (:SDL-KEY-LEFT
                            (setf (slot-value figure 'x-d) -1)
                            (move-figure figure arena :move-sideways t))
                           (:SDL-KEY-RIGHT
                            (setf (slot-value figure 'x-d) 1)
                            (move-figure figure arena :move-sideways t))
                           (:SDL-KEY-UP
                            (let ((tmp (make-instance 'figure
                                                    :x (x figure)
                                                    :y (y figure)
                                                    :color (color figure)
                                                    :body (body figure))))
                              (rotate-figure-clockwise tmp)
                              (unless (rotate-collision-p tmp arena)
                                (rotate-figure-clockwise figure))))))
        (:idle ()
               (when run
                 (when (> (- (sdl:system-ticks) ticks) (/ 1000 hz))
                   (when (move-figure figure arena)
                     (if (and (> (+ (slot-value figure 'y)
                              (array-dimension (slot-value figure 'body) 0))
                                 (slot-value arena 'height))
                              (not (zerop (slot-value figure 'y))))
                         (progn
                           (format t "~%Game Over!~%")
                           (sdl:push-quit-event))
                         (progn
                           (figure->arena figure arena)
                           (let ((lines (vanish-lines arena)))
                             (case lines
                               (4 (incf score 1000) (incf level-score 1000))
                               (3 (incf score 600) (incf level-score 600))
                               (2 (incf score 300) (incf level-score 300))
                               (1 (incf score 100) (incf level-score 100))))
                           (when (> level-score (* hz (* hz 100)))
                             (format t "~%Level ~d reached!~%" (+ 1 level))
                             (incf hz)
                             (incf level)
                             (setf level-score 0))
                           (setf figure (choose-figure figure arena))
                           (setf (slot-value figure 'color) (random-color))
                           (format t "   ~d   |   ~d   |   ~d~%"
                                   level hz score)
                           (finish-output))))
                   (setf ticks (sdl:system-ticks))))
               (draw-world arena figure)
               (sdl:update-display))))))

(defun make-executable ()
  #+sbcl (sb-ext:save-lisp-and-die "cl-tetris" :toplevel #'cl-tetris:run :executable t))
