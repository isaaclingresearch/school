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
  (execute-non-query *db* "create table levels (id INTEGER PRIMARY KEY, level TEXT, added_on DEFAULT CURRENT_TIMESTAMP)"))

(defun get-levels ()
  (execute-to-list *db* "select level from levels"))

(defun save-level (level)
  (execute-non-query *db* "insert into levels (level) values (?)" level))

(defun update-level (level new-level)
  (execute-non-query *db* "update levels set level = ? where level = ?" new-level level))

(defun delete-level (level)
  (execute-non-query *db* "delete from levels where level = ?" level))

(defparameter *menubar* nil)
(defun create-menubar ()
  "create a new menu bar, if an old pne exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (let* ((menubar (make-instance 'menubar))
	 (level-menu (make-instance 'menu :master menubar :text "Levels"))
	 (class-menu (make-instance 'menu :master menubar :text "Classes"))
	 (streams-menu (make-instance 'menu :master menubar :text "Streams")))
    (setq *menubar* menubar)
    ;; add elements to the menus.
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
					      (grid (make-instance 'label :master *school-info-main-frame* :text "The level has been delete. Note that this affects the data integrity.") 1 0)
					      ))))
    ))

(defun start ()
  "start the info application"
  (with-ltk ()
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
    (grid save-button 2 2 :pady 10)
    ))
