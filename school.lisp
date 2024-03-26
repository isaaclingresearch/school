(defpackage :school
  (:use :cl :ltk :sqlite)
  (:export :init :conn :db))

(in-package :school)

(defun init ()
  "this starts the application.
   creates the database if non exists.
   creates the relevant tables and populates with neccessary information."
  ())

(defmacro conn (&body body)
  `(with-open-database (db (uiop:native-namestring "~/common-lisp/school/db/school.db"))
     ,@body))
