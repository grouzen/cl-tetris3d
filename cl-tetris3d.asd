;;;; -*- Lisp -*-
;;;;
;;;; Clone of the original game a tetris.
;;;;
;;;; Author: Nedokushev Michael <michael.nedokushev@gmail.com>
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(defpackage #:cl-tetris3d-asd
  (:use :cl :asdf))

(in-package #:cl-tetris3d-asd)

(defsystem cl-tetris3d
  :name "cl-tetris3d"
  :version "0.0.1"
  :maintainer "Nedokushev Michael <michael.nedokushev@gmail.com>"
  :author "Nedokushev Michael <michael.nedokushev@gmail.com>"
  :license "MIT (also see COPYING file for details)"
  :description "Yet another 3D Tetris clone"
  :depends-on (#:cl-opengl #:lispbuilder-sdl #:cl-glu #:iterate)
  :serial t
  :components ((:file "package")
               (:file "tetris")))
