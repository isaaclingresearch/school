;; manages the time tables in the school.
(defpackage :school.timetable
  (:use :cl :sqlite :school :ltk :school.ltk :school.pdf :tktable :jonathan)
  (:shadow cl-pdf:image cl-pdf:make-image cl-pdf:font-metrics cl-pdf:bbox cl-pdf:name cl-pdf:scale str:repeat)
  (:export :start))


(in-package :school.timetable)

(defparameter *main-frame* nil)		
(defparameter *missing-details-message* "Some school details required to make PDFs are missing, please enter under School Details menu and try again.")
(defparameter *menubar* nil)

(defun start ()
  "start the timetable application, try to create the tables, bind the error to continue execution if the tables are already present. enable foreign key support on the database"
  (conn
    (execute-non-query db "pragma foreign_keys = on") 
    (handler-case ()
;	(create-tables db)
      (sqlite-error (err)
	(declare (ignore err))))
    )
  (with-ltk ()
    (iconphoto *tk* "~/common-lisp/school/static/logos/timetable.png")
   (create-menubar)
    (minsize *tk* 800 600)
    ;; start in maximized on OSX and Windows
    (unless (equal "Linux" (software-type))
      (setf (wm-state *tk*) 'zoomed))
    (wm-title *tk* "School Timetable")
    ))

(defun make-response (message)
  (create-menubar)
  (grid-columnconfigure *tk* 0 :weight 1) 
  (grid-rowconfigure *tk* 0 :weight 1)
  (when *main-frame*
    (destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (grid *main-frame* 0 0)
  (grid (make-instance 'label :master *main-frame* :text message) 1 0)
  )


(defun create-menubar ()
  "create a new menu bar, if an old one exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (setq *menubar* (make-instance 'menubar))
  (let* ((teachers (make-instance 'menu :master *menubar* :text "Teachers"))
	 (departments (make-instance 'menu :master *menubar* :text "Departments")))
    (declare (ignore teachers departments))
;    (teacher-menu teachers)
 ;   (department-menu departments)
    ))

