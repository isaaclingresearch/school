;;; this defines the application for handling general school information.
;;; general school information means the basic structure of the school.

;;; TOOD run the function to create tables on start, if db is already present, don't run those functions.
(defpackage :school-info
  (:use :cl :ltk :sqlite)
  (:export :start :create-tables))

(in-package :school-info)

(defparameter *school-info-main-frame* nil)		

;; defines the sqlite3 database used by the application.
(defparameter *db* (connect  (uiop:native-namestring "~/common-lisp/school/db/school.db")))

;; DB ACCESS FUNCTIONS

(defun create-tables ()
  (execute-non-query *db* "create table levels (id INTEGER PRIMARY KEY, level TEXT, added_on DEFAULT CURRENT_TIMESTAMP)")
 (execute-non-query *db* "create table classes (id INTEGER PRIMARY KEY, class TEXT, added_on DEFAULT CURRENT_TIMESTAMP)")
  (execute-non-query *db* "create table streams (id INTEGER PRIMARY KEY, class TEXT, stream TEXT, added_on DEFAULT CURRENT_TIMESTAMP)")
  )

;; LEVEL FUNCTIONS
(defun get-levels ()
  (execute-to-list *db* "select level from levels"))

(defun save-level (level)
  (execute-non-query *db* "insert into levels (level) values (?)" level))

(defun update-level (level new-level)
  (execute-non-query *db* "update levels set level = ? where level = ?" new-level level))

(defun delete-level (level)
  (execute-non-query *db* "delete from levels where level = ?" level))

;; CLASS FUNCTIONS
(defun get-classes ()
  (execute-to-list *db* "select class from classes"))

(defun save-class (class)
  (execute-non-query *db* "insert into classes (class) values (?)" class))

(defun update-class (class new-class)
  (execute-non-query *db* "update classes set class = ? where class = ?" new-class class))

(defun delete-class (class)
  (execute-non-query *db* "delete from classes where class = ?" class))

(defun iconbitmap (path-to-icon)		
  (format-wish "wm iconbitmap . ~a" path-to-icon))					

;; STREAM FUNCTIONS
(defun get-streams ()
  (execute-to-list *db* "select stream, class from streams"))

(defun save-stream (class stream)
  (execute-non-query *db* "insert into streams (class, stream) values (?, ?)" class stream))

(defun update-stream (class new-stream stream)
  (execute-non-query *db* "update streams set stream = ? where class = ? and stream = ?" new-stream class stream))

(defun delete-stream (class stream)
  (execute-non-query *db* "delete from streams where class = ? and stream = ?" class stream))

(defun iconbitmap (path-to-icon)		
  (format-wish "wm iconbitmap . ~a" path-to-icon))					

(defparameter *menubar* nil)
(defun create-menubar ()
  "create a new menu bar, if an old pne exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (let* ((menubar (make-instance 'menubar))
	 (level-menu (make-instance 'menu :master menubar :text "Levels"))
	 (class-menu (make-instance 'menu :master menubar :text "Classes"))
	 (stream-menu (make-instance 'menu :master menubar :text "Streams")))
    (setq *menubar* menubar)
    ;; add elements to the menus.
    ;; LEVEL MENUS
    (make-instance 'menubutton :master level-menu :text "New" :command (lambda () (show-add-level-form)))
    (let ((edit-level-menu (make-instance 'menu :master level-menu :text "Edit")))
      ;; for the edit-level-menu, make each saved level a button of the menu
      (dolist (level (get-levels))
	(make-instance 'menubutton :master edit-level-menu :text (car level) :command (lambda () (show-add-level-form (car level))))))
    ;; add buttons do delete all level to the delete level menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-level-menu (make-instance 'menu :master level-menu :text "Delete")))
      (dolist (level (get-levels))
	(make-instance 'menubutton :master delete-level-menu :text (car level)
				   :command (lambda ()
					      (delete-level (car level))
					      (create-menubar)
					      (grid-columnconfigure *tk* 0 :weight 1) 
					      (grid-rowconfigure *tk* 0 :weight 1)
					      (destroy *school-info-main-frame*)
					      (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
					      (grid *school-info-main-frame* 0 0)
					      (grid (make-instance 'label :master *school-info-main-frame* :text "The level has been deleted. Note that this affects the data integrity.") 1 0)
					      ))))

    ;; CLASSES MENU
    (make-instance 'menubutton :master class-menu :text "New" :command (lambda () (show-add-class-form)))
    (let ((edit-class-menu (make-instance 'menu :master class-menu :text "Edit")))
      ;; for the edit-class-menu, make each saved level a button of the menu
      (dolist (class (get-classes))
	(make-instance 'menubutton :master edit-class-menu :text (car class) :command (lambda () (show-add-class-form (car class))))))
    ;; add buttons do delete all level to the delete class menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-class-menu (make-instance 'menu :master class-menu :text "Delete")))
      (dolist (class (get-classes))
	(make-instance 'menubutton :master delete-class-menu :text (car class)
				   :command (lambda ()
					      (delete-class (car class))
					      (create-menubar)
					      (grid-columnconfigure *tk* 0 :weight 1) 
					      (grid-rowconfigure *tk* 0 :weight 1)
					      (destroy *school-info-main-frame*)
					      (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
					      (grid *school-info-main-frame* 0 0)
					      (grid (make-instance 'label :master *school-info-main-frame* :text "The class has been deleted. Note that this affects the data integrity.") 1 0)
					      ))))

     ;; STREAMS MENU
    ;; for the new stream, add all classes as menu buttons such that you add a stream to a class.
    (let ((new-stream-button (make-instance 'menu :master stream-menu :text "New")))
      (dolist (class (get-classes))
	(make-instance 'menubutton :master new-stream-button :text (car class) :command (lambda () (show-add-stream-form (car class))))))
   ; (make-instance 'menubutton :master stream-menu :text "New" :command (lambda () (show-add-stream-form)))
    (let ((edit-stream-menu (make-instance 'menu :master stream-menu :text "Edit")))
      ;; for the edit-stream-menu, make each saved level a button of the menu
      (dolist (stream (get-streams))
	(make-instance 'menubutton :master edit-stream-menu :text (car stream) :command (lambda () (show-add-stream-form (car stream) (cadr stream))))))
    ;; add buttons do delete all level to the delete stream menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-stream-menu (make-instance 'menu :master stream-menu :text "Delete")))
      (dolist (stream (get-streams))
	(make-instance 'menubutton :master delete-stream-menu :text (car stream)
				   :command (lambda ()
					      (delete-stream (car stream) (cadr stream))
					      (create-menubar)
					      (grid-columnconfigure *tk* 0 :weight 1) 
					      (grid-rowconfigure *tk* 0 :weight 1)
					      (destroy *school-info-main-frame*)
					      (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
					      (grid *school-info-main-frame* 0 0)
					      (grid (make-instance 'label :master *school-info-main-frame* :text "The stream has been deleted. Note that this affects the data integrity.") 1 0)
					      ))))
    ))

(defun start ()
  "start the info application"
  (with-ltk ()
   ; (iconbitmap #p"/home/lam/common-lisp/school/favicon.ico")
    (create-menubar)
    (minsize *tk* 800 600)
    ;; start in maximized on OSX and Windows
    (unless (equal "Linux" (software-type))
      (setf (wm-state *tk*) 'zoomed))
    (wm-title *tk* "School Info")
    ))

(defun show-add-level-form (&optional level-text)
  "collect and process data about levels"
  (unless (null *school-info-main-frame*)
    (ltk:destroy *school-info-main-frame*))
  (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (let* (
	 (level-label (make-instance 'label :master *school-info-main-frame* :text "Enter Level Name"))
	 (level-entry (make-instance 'entry :master *school-info-main-frame* :text level-text))
	 (save-button (make-instance 'button :master *school-info-main-frame*
					     :text "Save Level" :command (lambda ()
									   (if level-text
									       (update-level level-text (text level-entry))
									       (save-level (text level-entry)))
									   (create-menubar)
									   (destroy *school-info-main-frame*)
									   (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
									   (grid *school-info-main-frame* 0 0)
									   (grid (make-instance 'label :master *school-info-main-frame* :text "The level has been saved.") 1 0)))))
    (grid *school-info-main-frame* 0 0)
    (grid-columnconfigure *tk* 0 :weight 1) 
    (grid-rowconfigure *tk* 0 :weight 1)
    (grid  level-label 1 0 :padx 10 :pady 5)
    (grid level-entry 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
    (grid save-button 2 2 :pady 10)))

(defun show-add-class-form (&optional class-text)
  "collect and process data about classes
   the form has a dropdown list of levels to choose from, shows an error if no levels are present."
  (let ((level)
	(levels (get-levels)))   
    (unless (null *school-info-main-frame*)
      (ltk:destroy *school-info-main-frame*))
    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
    (let* (
	   (level-label (make-instance 'label :master *school-info-main-frame* :text "Select Level"))
	   (level-combobox (make-instance 'combobox :text (caar levels) :master *school-info-main-frame* :values (mapcar (lambda (x) (car x)) levels)))
	   (class-label (make-instance 'label :master *school-info-main-frame* :text "Enter Class Name"))
	   (class-entry (make-instance 'entry :master *school-info-main-frame* :text class-text))
	   (save-button (make-instance 'button :master *school-info-main-frame*
					       :text "Save Class" :command (lambda ()
									     (if class-text
										 (update-class class-text (text class-entry))
										 (save-class (text class-entry)))
									     (create-menubar)
									     (destroy *school-info-main-frame*)
									     (format *standard-output* "~a" (text level-combobox))
									     (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
									     (grid *school-info-main-frame* 0 0)
									     (grid (make-instance 'label :master *school-info-main-frame* :text "The class has been saved.") 1 0)))))
      (grid *school-info-main-frame* 0 0)
      (grid-columnconfigure *tk* 0 :weight 1) 
      (grid-rowconfigure *tk* 0 :weight 1)
      (grid level-label 0 0 :padx 10 :pady 5)
      (grid level-combobox 0 2 :padx 10 :pady 5)
      (grid class-label 1 0 :padx 10 :pady 5)
      (grid class-entry 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
      (grid save-button 2 2 :pady 10))))

(defun show-add-stream-form (&optional class-text stream-text )
  "collect and process data about streams"
  (unless (null *school-info-main-frame*)
    (ltk:destroy *school-info-main-frame*))
  (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (let* (	 
	 (class-label (make-instance 'label :master *school-info-main-frame* :text "Enter Class Name"))
	 (class-entry (make-instance 'entry :master *school-info-main-frame* :text class-text :state (if class-text :readonly :normal)))
	 (stream-label (make-instance 'label :master *school-info-main-frame* :text "Enter Stream Name"))
	 (stream-entry (make-instance 'entry :master *school-info-main-frame* :text stream-text))
	 (save-button (make-instance 'button :master *school-info-main-frame*
					     :text "Save Stream" :command (lambda ()
									    (if stream-text
										(update-stream class-text stream-text (text stream-entry))
										(save-stream (text class-entry) (text stream-entry)))
									    (create-menubar)
									    (destroy *school-info-main-frame*)
									    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
									    (grid *school-info-main-frame* 0 0)
									    (grid (make-instance 'label :master *school-info-main-frame* :text "The stream has been saved.") 1 0)))))
    (grid *school-info-main-frame* 0 0)
    (grid-columnconfigure *tk* 0 :weight 1) 
    (grid-rowconfigure *tk* 0 :weight 1)
    (grid class-label 1 0 :padx 10 :pady 5)
    (grid class-entry 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
    (grid stream-label 2 0 :padx 10 :pady 5)
    (grid stream-entry 2 1 :padx 10 :pady 5 :sticky "e" :columnspan 5)
    (grid save-button 3 2 :pady 10)))
