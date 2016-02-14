;;;; -*- Lisp -*-
;;;;
;;;; Clone of the original game a tetris.
;;;;
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>
;;;;
;;;; This file released under some license restrictions,
;;;; see COPYING file.

(defpackage #:cl-tetris3d-asd
  (:use :cl :asdf))

(defsystem cl-tetris3d
  :name "cl-tetris3d"
  :version "0.0.0"
  :maintainer "Nedokushev Michael <grouzen.hexy@gmail.com>"
  :author "Nedokushev Michael <grouzen.hexy@gmail.com>"
  :license "MIT (also see COPYING file for details)"
  :description "Clone of the original game a tetris"
  :depends-on (#:cl-opengl #:lispbuilder-sdl #:cl-glu)
  :serial t
  :components ((:file "package")
               (:file "tetris")))
