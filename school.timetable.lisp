;; manages the time tables in the school.
(defpackage :school.timetable
  (:use :cl :sqlite :school :ltk :school.ltk :school.pdf :school.info :tktable :jonathan)
  (:shadow cl-pdf:image cl-pdf:make-image cl-pdf:font-metrics cl-pdf:bbox cl-pdf:name cl-pdf:scale str:repeat school.info:start)
  (:export :start))


(in-package :school.timetable)

(defparameter *main-frame* nil)		
(defparameter *missing-details-message* "Some school details required to make PDFs are missing, please enter under School Details menu and try again.")
(defparameter *menubar* nil)

(defun create-tables (database)
  (conn
    ;; template is json of conses having timeslots in ascending order (list (1 (start1 end1)) ...)
    ;; the name can be a name of a level or a class, type is either class or level
    (execute-non-query database "create table templates (id integer primary key, type text, name text unique, template text)"))
  )

(defun save-template (type name timeslots)
  (conn
    (execute-non-query db "insert or replace into templates (type, name, template) values (?,?,?)" type name (to-json timeslots))))

(defun get-all-templates ()
  (conn (execute-to-list db "select * from templates")))

(defun get-template-by-name (name)
  (conn (execute-to-list db "select * from templates where name = ?" name)))

(defun start ()
  "start the timetable application, try to create the tables, bind the error to continue execution if the tables are already present. enable foreign key support on the database"
  (conn
    (execute-non-query db "pragma foreign_keys = on") 
    (handler-case
	(create-tables db)
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

(defun prepare-main-frame ()
  (grid *main-frame* 0 0)
  (format-wish "grid anchor ~a nw" (widget-path *tk*))
  (grid-columnconfigure *tk* 0 :weight 1) 
  (grid-rowconfigure *tk* 0 :weight 1))

(defun center-main-frame ()
  (grid *main-frame* 0 0 :sticky "nwes")
  (format-wish "grid anchor . center")
  (grid-columnconfigure *main-frame* 0 :weight 1) 
  (grid-rowconfigure *main-frame* 0 :weight 1))

(defun make-response (message)
  (create-menubar)
  (grid-columnconfigure *tk* 0 :weight 1) 
  (grid-rowconfigure *tk* 0 :weight 1)
  (when *main-frame*
    (destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (grid *main-frame* 0 0)
  (grid (make-instance 'label :master *main-frame* :text message) 1 0))


(defun create-menubar ()
  "create a new menu bar, if an old one exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (setq *menubar* (make-instance 'menubar))
  (let* ((templates (make-instance 'menu :master *menubar* :text "Templates"))
	 (timetables (make-instance 'menu :master *menubar* :text "Timetables"))
	 (departments (make-instance 'menu :master *menubar* :text "Departments")))
    (declare (ignore departments))
    (templates-menu templates)
    (timetables-menu timetables)
 ;   (department-menu departments)
    ))

(defun timetables-menu (menu)
  "this will display a menu of classes each of which is also a menu of streams"
  (dolist (class (school.info::get-classes))
    (let ((class-menu (make-instance 'menu :master menu :text (cadr class))))
      (dolist (stream (school.info::get-streams (car class)))
	(make-instance 'menubutton :text (cadr stream) :master class-menu :command (lambda () (create-timetable class stream)))))))

(defun create-timetable (class stream)
  "function displays a form for creating a timetable, if one is already saved, the fields are populated with an existing timetable.
   the timeslots are from templates"
   (unless (null  *main-frame*)
    (ltk:destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (center-main-frame)
  (wm-title *tk* (format nil "~a ~a timetable " (cadr class) (cadr stream)))
  ;; get the template, if the class has a template, get that, else get the level template
  (let* ((level (school.info::|get-level| (third class)))
	(saved-template (let ((local-template (get-template-by-name (cadr class))))
			  (if local-template
			      local-template
			      (get-template-by-name (cadar level))))))
    (if saved-template
	(let* ((saved-slots (parse (fourth (car saved-template))))
	       (title-cols (mapcar (lambda (i) (format nil "~a - ~a" (first i) (second i))) saved-slots))
	       (table (make-instance 'scrolled-table :titlecols 1
						     :titlerows 1
						     :data (list '("MON" "TUE" "WED" "THUR" "FRI" "SAT")
								 title-cols))))
	  (grid table 0 0))
	(make-response (format nil "No template for ~a ~a was found. Add a template and try again" (cadr class) (cadr stream))))))

(defun templates-menu (menu)
  "display buttons for templates for levels, separated by a line and those for classes. 
   the templates will be used for actual timetables."
  (dolist (level (school.info:|get-level|))
    (make-instance 'menubutton :text (cadr level) :master menu :command (lambda () (create-template :level level))))
  (add-separator menu)
  (dolist (class (school.info::get-classes))
    (make-instance 'menubutton :text (cadr class) :master menu :command (lambda () (create-template :class class)))))

(defun create-template (&key class level)
  "create a template, either a class or a level is provided."
  (unless (null  *main-frame*)
    (ltk:destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (center-main-frame)
  (wm-title *tk* (format nil "~a timetable template" (cadr (or class level))))
  (let* (timeslots
	 ;; if a class is supplied, but its no template is saved for it, use it's level's template.
	 (saved-template (if class
			     (let ((local-template (get-template-by-name (cadr class))))
			       (if local-template
				   local-template
				   (get-template-by-name (cadar (school.info::|get-level| (third class))))))
			     (get-template-by-name (cadr level))))
	 (slot-entry (make-instance 'entry :master *main-frame* :text (if saved-template (length (parse (nth 3 (car saved-template)))) 8))))
    (grid (make-instance 'button :master *main-frame* :text "Save template"
				 :command (lambda ()
					    "collect all the data from the timeslots as cons, convert them to json and save that as a level or class template"
					    (let (saved-ts)
					      (print timeslots)
					      (dolist (ts timeslots)
						(setq saved-ts `(,@saved-ts ,(list (text (cadr ts)) (text (caddr ts))))))
					      (save-template 
					       (if class "class" "level")
					       (cadr (or class level))
					       (remove '("" "") saved-ts :test #'equal)))
					    (make-response (format nil "~a timetable template has been saved." (cadr (or class level))))))
	  0 1)
    ;; create an initial 8 timeslots for a timetable, with a button to add more
    (grid (make-instance 'label :master *main-frame* :text "Number of time slots") 1 0)
    (grid slot-entry 1 1)
    (grid (make-instance 'button :master *main-frame* :text "Change slot number"
				 :command (lambda ()
					    "get the entered slot count, if it's greater than currently displayed, increase them else decrease them."
					    (let ((slot-count (parse (text slot-entry))))
					      (cond ((> slot-count (length timeslots))
						     (setq timeslots
							   `(,@(loop for i from (length timeslots) to (1- slot-count)
								    collect (let ((ts-start (make-instance 'entry :master *main-frame*))
										  (ts-end (make-instance 'entry :master *main-frame*))
										  (ts-label (make-instance 'label :master *main-frame* :text (format nil "Time slot ~a" (1+ i)))))
									      (grid ts-label (+ 3 i) 0)
									      (grid ts-start (+ 3 i) 1)
									      (grid ts-end (+ 3 i) 2)
									      (list ts-label ts-start ts-end)))
							     ,@timeslots)
							   ))
						    ((< slot-count (length timeslots))
						     (let ((timeslot-length (length timeslots)))
						       (loop for i from slot-count below timeslot-length
							     do (let ((timeslot (nth (- timeslot-length i) timeslots)))	
								  (ltk:destroy (first timeslot))
								  (ltk:destroy (second timeslot))
								  (ltk:destroy (third timeslot)))))
						     (setq timeslots (subseq timeslots 0 slot-count))
						     )
						    (t)))))
	  1 2)
    (grid (make-instance 'label :master *main-frame* :text "Slot") 2 0)
    (grid (make-instance 'label :master *main-frame* :text "Slot Start") 2 1)
    (grid (make-instance 'label :master *main-frame* :text "Slot End") 2 2)
    (setq timeslots ;; update the timeslots list to be used later to get and update values of timeslots
	  (if saved-template
	      ;; if there is some template that was saved, use that
	      (let ((saved-slots (parse (nth 3 (car saved-template)))))
		(loop for i from 0 below (length saved-slots)
		      collect (let* ((ts-slot (nth i saved-slots))
				     (ts-start (make-instance 'entry :master *main-frame* :text (first ts-slot)))
				     (ts-end (make-instance 'entry :master *main-frame* :text (second ts-slot)))
				     (ts-label (make-instance 'label :master *main-frame* :text (format nil "Time slot ~a" (1+ i)))))
				(grid  ts-label (+ 3 i) 0)
				(grid ts-start (+ 3 i) 1)
				(grid ts-end (+ 3 i) 2)
				(list ts-label ts-start ts-end))))
	      (loop for i from 3 to 10
		    collect (let ((ts-start (make-instance 'entry :master *main-frame*))
				  (ts-end (make-instance 'entry :master *main-frame*))
				  (ts-label (make-instance 'label :master *main-frame* :text (format nil "Time slot ~a" (1- i)))))
			      (grid  ts-label i 0)
			      (grid ts-start i 1)
			      (grid ts-end i 2)
			      (list ts-label ts-start ts-end))))
	  )
    ))
