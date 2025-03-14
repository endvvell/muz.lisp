(defpackage #:colors
  (:use #:cl)
  (:export #:RED
           #:RED_WITH_UNDERLINE
           #:GREEN
           #:GREEN_WITH_UNDERLINE
           #:YELLOW
           #:YELLOW_WITH_UNDERLINE
           #:BLUE
           #:BLUE_WITH_UNDERLINE
           #:RESET_COLOR))

(in-package #:colors)

(defvar RED (format nil "~c[31m" #\ESC))

(defvar RED_WITH_UNDERLINE (format nil "~c[4;31m" #\ESC))

(defvar GREEN (format nil "~c[32m" #\ESC))

(defvar GREEN_WITH_UNDERLINE (format nil "~c[4;32m" #\ESC))

(defvar YELLOW (format nil "~c[33m" #\ESC))

(defvar YELLOW_WITH_UNDERLINE (format nil "~c[4;33m" #\ESC))

(defvar BLUE (format nil "~c[34m" #\ESC))

(defvar BLUE_WITH_UNDERLINE (format nil "~c[4;34m" #\ESC))

(defvar RESET_COLOR (format nil "~c[0m" #\ESC))
