;; this file contains code that extends the functionality of ltk,
;; uses ltk provided functions and macros to add tcl/tk functionality not present in the original ltk.

(defpackage :school.ltk
  (:use :cl :ltk)
  (:export :iconphoto))

(in-package :school.ltk)

(defgeneric iconphoto (widget image-path))
(defmethod iconphoto ((w widget) image-path)
  ;; first create the photo in tk with a name logo, then supply the name to the window to display it.
  (format-wish "image create photo logo -file ~a -format png; wm iconphoto ~a logo" image-path (widget-path w))
  w)
