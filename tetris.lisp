;;;; Clone of the original game "Tetris".
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(in-package :cl-tetris3d)

(defun gl-init (width height)
  (progn
    (gl:clear-color 0.1 0.1 0.1 0)
    (gl:clear-depth 1)
    (gl:enable :depth-test)
    (gl:enable :polygon-offset-fill)
    (gl:polygon-offset 2 1)
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 60 (/ width height) 0.5 100)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defun random-color ()
  (let ((color (random 3))
        (colors (list :red :yellow :blue)))
    (nth color colors)))

(defmacro do-dotimes-twice ((h w) &body body)
  `(dotimes (h ,h)
     (dotimes (w ,w)
       ,@body)))

(defun build-cube (height width)
  (let ((h (/ height 2))
        (w (/ width 2)))
    (gl:push-matrix)
    (gl:with-primitives :quads
      (gl:vertex (- -1 w) (- -1 h) 1)
      (gl:vertex (- 1 w) (- -1 h) 1)
      (gl:vertex (- 1 w) (- 1 h) 1)
      (gl:vertex (- -1 w) (- 1 h) 1)

      (gl:vertex (- 1 w) (- -1 h) 1)
      (gl:vertex (- 1 w) (- -1 h) -1)
      (gl:vertex (- 1 w) (- 1 h) -1)
      (gl:vertex (- 1 w) (- 1 h) 1)

      (gl:vertex (- 1 w) (- -1 h) -1)
      (gl:vertex (- 1 w) (- 1 h) -1)
      (gl:vertex (- -1 w) (- 1 h) -1)
      (gl:vertex (- -1 w) (- -1 h) -1)

      (gl:vertex (- -1 w) (- -1 h) -1)
      (gl:vertex (- -1 w) (- 1 h) -1)
      (gl:vertex (- -1 w) (- 1 h) 1)
      (gl:vertex (- -1 w) (- -1 h) 1)

      (gl:vertex (- -1 w) (- -1 h) 1)
      (gl:vertex (- 1 w) (- -1 h) 1)
      (gl:vertex (- 1 w) (- -1 h) -1)
      (gl:vertex (- -1 w) (- -1 h) -1)

      (gl:vertex (- -1 w) (- 1 h) 1)
      (gl:vertex (- 1 w) (- 1 h) 1)
      (gl:vertex (- 1 w) (- 1 h) -1)
      (gl:vertex (- -1 w) (- 1 h) -1))
    (gl:pop-matrix)))

(defclass figure ()
  ((x
    :initarg :x
    :accessor x
    :initform 0)
   (y
    :initarg :y
    :accessor y
    :initform 0)
   (x-d
    :initarg :x-d
    :accessor x-d
    :initform 0)
   (color
    :initarg :color
    :accessor color
    :initform (random-color))
   (body
    :initarg :body
    :accessor body
    :initform nil)))

(defclass arena ()
  ((width
    :initarg :width
    :accessor width
    :initform 8)
   (height
    :initarg :height
    :accessor height
    :initform 12)
   (field
    :initarg :field
    :initform (make-array '(12 8) :initial-element nil)
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
      (let ((prev-body body))
        (setf body (make-array (list (array-dimension prev-body 1)
                                     (array-dimension prev-body 0))))
	(iter (for prev-w from 0)
	      (for h from (1- (array-dimension body 0)) downto 0)
	      (iter (for prev-h from 0)
		    (for w from 0 to (1- (array-dimension prev-body 0)))
		    (setf (aref body h w) (aref prev-body prev-h prev-w))))))))

(defgeneric rotate-figure-counterclockwise (figure)
  (:method ((figure figure))
    (with-slots (body) figure
      (let ((prev-body body))
        (setf body (make-array (list (array-dimension prev-body 1)
                                     (array-dimension prev-body 0))))
	(iter (for prev-w from 0)
	      (for h from 0 to (1- (array-dimension body 0)))
	      (iter (for prev-h from 0)
		    (for w from (1- (array-dimension prev-body 0)) downto 0)
		    (setf (aref body h w) (aref prev-body prev-h prev-w))))))))

(defgeneric draw-arena (arena)
  (:method ((arena arena))
    (with-slots (width height field) arena
      (do-dotimes-twice (height width)
        (gl:push-matrix)
        (gl:translate (* w 2) (* h 2) 0)
        (let ((cell (aref field h w)))
          (when (not (null cell))
            (gl:polygon-mode :front-and-back :fill)
            (case cell
              (:red (gl:color 1 0 0))
              (:yellow (gl:color 1 1 0))
              (:blue (gl:color 0 0 1)))
            (build-cube height width)))
        (gl:color 0 1 0)
        (gl:polygon-mode :front-and-back :line)
        (build-cube height width)
        (gl:pop-matrix)))))

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

(defgeneric draw-world (arena figure &key x y z)
  (:method ((arena arena) (figure figure) &key x y z)
    (with-slots (width height) arena
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:push-matrix)
      (gl:load-identity)
      (gl:translate (- (/ width 2)) (- (/ height 2)) z)
      (gl:rotate x 1 0 0)
      (gl:rotate y 0 1 0)
      (draw-arena arena)
      (draw-figure figure arena)
      (gl:pop-matrix)
      (gl:flush))))

(defgeneric draw-figure (figure arena)
  (:method ((figure figure) (arena arena))
    (with-slots (x y color body) figure
      (with-slots (width height) arena
        (case color
          (:red (gl:color 1 0 0))
          (:yellow (gl:color 1 1 0))
          (:blue (gl:color 0 0 1)))
        (gl:polygon-mode :front-and-back :fill)
        (do-dotimes-twice ((array-dimension body 0) (array-dimension body 1))
          (when (= (aref body h w) 1)
            (gl:push-matrix)
            (gl:translate (+ (* w 2) (* x 2)) (+ (* h 2) (* y 2)) 0)
            (build-cube height width)
            (gl:pop-matrix)))))))
  
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
    (let ((choise (random 7)))
      (with-slots (x y) figure
        (with-slots (width height) arena
          (setf x (1- (floor (/ width 2)))
                y height)))
      (setf (body figure) (nth choise *figures*))
      figure)))

(defun run (&key (width 480) (height 640) (bpp 32))
    (sdl:with-init ()
      (unless (sdl:window width height
                          :bpp bpp
                          :opengl t
                          :opengl-attributes '((:sdl-gl-doublebuffer 1)))
        (error "~&Unable to create a SDL window~%"))
      (setf (sdl:frame-rate) 40)
      (sdl:enable-key-repeat 50 50)
      (gl-init width height)

      ;; Print "Key Bindings"
      (format t "Key Bindings:~%~
                 a:         Move current figure to the λeft ~%~
                 d:         Move current figure to the right ~%~
                 space:     Drop down current figure immediateλy ~%~
                 λeft:      Rotate the camera λeft ~%~
                 right:     Rotate the camera right ~%~
                 down:      Rotate the camera down ~%~
                 up:        Rotate the camera up ~%~
                 page down: Zoom in the camera ~%~
                 page up:   Zoom out the camera~%~%")
      (format t "Get ready! We start the game!~%~%")
      
      (let* ((arena
              (make-instance 'arena
                             :width 10
                             :height 18
                             :field (make-array '(18 10) :initial-element nil)))
             (figure (choose-figure (make-instance 'figure) arena))
             (ticks (sdl:system-ticks))
             (run t)
             (score 0)
             (level-score 0)
             (hz 3)
             (level 0)
             (z -42)
             (x 8)
             (y -18))
        (sdl:with-events ()
          (:quit-event () t)
          (:key-down-event (:key key)
                           (cond ((eq key :SDL-KEY-ESCAPE)
                                  (sdl:push-quit-event))
                                 ((eq key :SDL-KEY-p)
                                  (if run
                                      (setf run nil)
                                      (setf run t)))
                                 ((eq key :SDL-KEY-PAGEUP)
                                  (unless (> (+ z 2) -38)
                                    (setf z (+ z 2))))
                                 ((eq key :SDL-KEY-PAGEDOWN)
                                  (when (> (- z 2) -80)
                                    (setf z (- z 2))))
                                 ((eq key :SDL-KEY-RIGHT)
                                  (setf y (+ y 2)))
                                 ((eq key :SDL-KEY-LEFT)
                                  (setf y (- y 2)))
                                 ((eq key :SDL-KEY-UP)
                                  (setf x (+ x 2)))
                                 ((eq key :SDL-KEY-DOWN)
                                  (setf x (- x 2)))
                                 ((eq key :SDL-KEY-SPACE)
                                  (do ()
                                      ((move-figure figure arena))))
                                 ((eq key :SDL-KEY-a)
                                  (setf (slot-value figure 'x-d) -1)
				  (move-figure figure arena :move-sideways t))
                                 ((eq key :SDL-KEY-d)
                                  (setf (slot-value figure 'x-d) 1)
				  (move-figure figure arena :move-sideways t))
                                 ((eq key :SDL-KEY-s)
                                  (move-figure figure arena))
                                 ((eq key :SDL-KEY-e)
                                  (with-slots (x y color body) figure
                                    (let ((tmp (make-instance 'figure
                                                              :x x
                                                              :y y
                                                              :color color
                                                              :body body)))
                                      (rotate-figure-clockwise tmp)
                                      (unless (rotate-collision-p tmp arena)
                                        (rotate-figure-clockwise figure)))))
                                 ((eq key :SDL-KEY-q)
                                  (with-slots (x y color body) figure
                                    (let ((tmp (make-instance 'figure
                                                              :x x
                                                              :y y
                                                              :color color
                                                              :body body)))
                                      (rotate-figure-counterclockwise tmp)
                                      (unless (rotate-collision-p tmp arena)
                                        (rotate-figure-counterclockwise figure)))))
				 ))
          
          (:idle ()
                 (when run
                   (when (> (- (sdl:system-ticks) ticks) (/ 1000 hz))
                     (when (move-figure figure arena)
                       (if (> (+ (slot-value figure 'y)
                                 (array-dimension (slot-value figure 'body) 0))
                              (slot-value arena 'height))
                           (progn
                             (format t "~%ᴪᴪᴪ Game Over ᴪᴪᴪ~%")
                             (sdl:push-quit-event))
                           (progn
                             (figure->arena figure arena)
                             (let ((lines (vanish-lines arena)))
                               (case lines
                                 (4 (progn (incf score 1000)
                                           (incf level-score 1000)))
                                 (3 (progn (incf score 600)
                                           (incf level-score 600)))
                                 (2 (progn (incf score 300)
                                           (incf level-score 300)))
                                 (1 (progn (incf score 100)
                                           (incf level-score 100)))))
                             (when (> level-score (* hz (* hz 100)))
                               (format t "~%You have reached λevel ~d! Congratuλations!~%~%" (+ level 1))
                               (incf hz)
                               (incf level)
                               (setf level-score 0))
                             (setf figure (choose-figure figure arena))
                             (setf (slot-value figure 'color) (random-color))
                             (format t "λevel: ~d | speed: ~d | score: ~d.~%"
                                     level hz score)
                             (finish-output))))
                     (setf ticks (sdl:system-ticks))))
                 (draw-world arena figure :x x :y y :z z)
                 (sdl:update-display))))))

(defun make-executable ()
  #+sbcl (sb-ext:save-lisp-and-die "cl-tetris3d" :toplevel #'cl-tetris3d:run :executable t))
