;;; this defines the application for handling general school information.
;;; general school information means the basic structure of the school.

;;; TOOD use foreign keys in the tables to ensure data integrity
(defpackage :school-info
  (:use :cl :ltk :sqlite)
  (:export :start :create-tables))

(in-package :school-info)

(defparameter *school-info-main-frame* nil)		

;; defines the sqlite3 database used by the application.
(defparameter *db* (connect  (uiop:native-namestring "~/common-lisp/school/db/school.db")))

;; DB ACCESS FUNCTIONS

(defun create-tables ()
  (execute-non-query *db* "create table levels (id integer primary key, level text uniqueE, added_on default current_timestamp)")
  (execute-non-query *db* "create table classes (id integer primary key, class text unique,  level_id integer, added_on default current_timestamp, foreign key (level_id) references levels (id))")
  (execute-non-query *db* "create table streams (id integer primary key, class_id integer, stream text unique, added_on default current_timestamp, foreign key (class_id) references classes (id))")
  (execute-non-query *db* "create table houses (id integer primary key, house text unique, added_on default current_timestamp)")
  ;; use stream is to ensure uniqueness as different classes can have the same stream
  (execute-non-query *db* "create table subjects (id integer primary key, subject text unique, stream_id integer, added_on default current_timestamp, foreign key (stream_id) references streams (id))")
  )

;; LEVEL FUNCTIONS
(defun get-levels ()
  (execute-to-list *db* "select level, id from levels"))

(defun get-level-id (level)
  (caar (execute-to-list *db* "select id from levels where level = ?" level)))

(defun save-level (level)
  (execute-non-query *db* "insert into levels (level) values (?)" level))

(defun update-level (level new-level)
  (execute-non-query *db* "update levels set level = ? where level = ?" new-level level))

(defun delete-level (level)
  (execute-non-query *db* "delete from levels where level = ?" level))

;; CLASS FUNCTIONS
(defun get-classes (&optional level-id)
  (if level-id
      (execute-to-list *db* "select class, id from classes where level_id = ?" level-id)
      (execute-to-list *db* "select class, level_id from classes")))

(defun get-class-id (class)
  (caar (execute-to-list *db* "select id from classes where class = ?" class)))

(defun get-class-name (class-id)
  (caar (execute-to-list *db* "select class from classes where id = ?" class-id)))

(defun save-class (level-id class)
  (execute-non-query *db* "insert into classes (level_id, class) values (?, ?)" level-id class))

(defun update-class (level-id class new-class)
  (execute-non-query *db* "update classes set class = ? where class = ? and level_id = ?" new-class class level-id))

(defun delete-class (level-id class)
  (execute-non-query *db* "delete from classes where level_id = ? and class = ?" level-id class))

(defun iconbitmap (path-to-icon)		
  (format-wish "wm iconbitmap . ~a" path-to-icon))					

;; STREAM FUNCTIONS
(defun get-streams (&optional class-id)
  (if class-id
      (execute-to-list *db* "select stream, id, class_id from streams where class_id = ?" class-id)
      (execute-to-list *db* "select stream, class_id, id from streams")))

(defun save-stream (class-id stream)
  (execute-non-query *db* "insert into streams (class_id, stream) values (?, ?)" class-id stream))

(defun update-stream (class-id stream new-stream)
  (execute-non-query *db* "update streams set stream = ? where class_id = ? and stream = ?" new-stream class-id stream))

(defun delete-stream (class-id stream)
  (execute-non-query *db* "delete from streams where class_id = ? and stream = ?" class-id stream))

;; HOUSE FUNCTIONS
(defun get-houses ()
  (execute-to-list *db* "select house, class from houses"))

(defun save-house (house)
  (execute-non-query *db* "insert into houses (house) values (?)" house))

(defun update-house (house new-house)
  (execute-non-query *db* "update houses set house = ? where house = ?" new-house house))

(defun delete-house (house)
  (execute-non-query *db* "delete from houses where house = ?" house))

;; SUBJECT FUNCTIONS
(defun get-subjects ()
  (execute-to-list *db* "select id, subject, stream_id from subjects"))

(defun get-stream-subjects (stream-id)
  (execute-to-list *db* "select id, subject from subjects where stream_id = ?" stream-id))

(defun save-subject (stream-id subject)
  (execute-non-query *db* "insert into subjects (stream_id, subject) values (?, ?)" stream-id subject))

(defun update-subject (subject-id subject)
  (execute-non-query *db* "update subjects set subject = ? where id = ?" subject subject-id))

(defun delete-subject (subject-id)
  (execute-non-query *db* "delete from subjects where id = ?" subject-id))

(defun iconbitmap (path-to-icon)		
  (format-wish "wm iconbitmap . ~a" path-to-icon))					

(defparameter *menubar* nil)

(defun prepare-main-window ()
  (grid *school-info-main-frame* 0 0)
  (grid-columnconfigure *tk* 0 :weight 1) 
  (grid-rowconfigure *tk* 0 :weight 1))

(defun create-menubar ()
  "create a new menu bar, if an old one exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (let* ((menubar (make-instance 'menubar))
	 (level-menu (make-instance 'menu :master menubar :text "Levels"))
	 (class-menu (make-instance 'menu :master menubar :text "Classes"))
	 (stream-menu (make-instance 'menu :master menubar :text "Streams"))
	 (subject-menu (make-instance 'menu :master menubar :text "Subjects"))
	 (paper-menu (make-instance 'menu :master menubar :text "Papers"))
	 (house-menu (make-instance 'menu :master menubar :text "Houses"))
	 (dormitory-menu (make-instance 'menu :master menubar :text "Dormitories"))
	 )
    (setq *menubar* menubar)
    ;; add elements to the menus.
    ;; LEVEL MENUS
    (make-instance 'menubutton :master level-menu :text "New" :command (lambda () (level-form)))
    (let ((edit-level-menu (make-instance 'menu :master level-menu :text "Edit")))
      ;; for the edit-level-menu, make each saved level a button of the menu
      (dolist (level (get-levels))
	(make-instance 'menubutton :master edit-level-menu :text (car level) :command (lambda () (level-form (car level))))))
    ;; add buttons do delete all level to the delete level menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-level-menu (make-instance 'menu :master level-menu :text "Delete")))
      (dolist (level (get-levels))
	(make-instance 'menubutton :master delete-level-menu :text (car level)
				   :command (lambda ()
					      (let ((message-text "Level has been deleted."))
						(handler-case (delete-level (car level))
						  (sqlite-constraint-error (err)
						    (declare (ignorable err))
						    (setq message-text "Level can't be deleted as it has classes, first delete them and try again.")))
						(create-menubar)
						(grid-columnconfigure *tk* 0 :weight 1) 
						(grid-rowconfigure *tk* 0 :weight 1)
						(destroy *school-info-main-frame*)
						(setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
						(grid *school-info-main-frame* 0 0)
						(grid (make-instance 'label :master *school-info-main-frame* :text message-text) 1 0))))))

    ;; CLASSES MENU
    (make-instance 'menubutton :master class-menu :text "New" :command (lambda () (class-form)))
    (let ((edit-class-menu (make-instance 'menu :master class-menu :text "Edit"))
	  (levels (get-levels)))
      ;; get all levels, then classes of a certain level,
      ;; display levels as menus with classes as menubuttons with command to edit class.
      (dolist (level levels)
	(let* ((level-name (car level))
	       (level-id (cadr level))
	       (level-menu (make-instance 'menu :master edit-class-menu :text level-name))
	       (classes (get-classes level-id)))
	  (dolist (class classes)
	    (make-instance 'menubutton :master level-menu :text (car class) :command (lambda () (class-form level-id (car class))))))))
    ;; add buttons do delete all level to the delete class menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-class-menu (make-instance 'menu :master class-menu :text "Delete"))
	  (levels (get-levels)))
      ;; get all levels, then classes of a certain level
      ;; display levels as menus with classes as menubuttons with a command to delete class
      (dolist (level levels)
	(let*  ((level-id (cadr level))
		(level-name (car level))
		(level-menu (make-instance 'menu :master delete-class-menu :text level-name)))
	  (dolist (class (get-classes level-id))
	    (make-instance 'menubutton :master level-menu :text (car class)
				       :command (lambda ()
						  (let ((message "The class has been deleted.")) 
						    (handler-case (delete-class level-id (car class))
						      (sqlite-constraint-error (err)
							(declare (ignorable err))
							(setq message "Class can't be as it has streams, first delete them and try again.")))
						    (create-menubar)
						    (grid-columnconfigure *tk* 0 :weight 1) 
						    (grid-rowconfigure *tk* 0 :weight 1)
						    (destroy *school-info-main-frame*)
						    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
						    (grid *school-info-main-frame* 0 0)
						    (grid (make-instance 'label :master *school-info-main-frame* :text message) 1 0))
						  ))))))

    ;; STREAMS MENU
    ;; for the new stream, add all classes as menu buttons such that you add a stream to a class.
    (make-instance 'menubutton :master stream-menu :text "New" :command (lambda () (stream-form)))
    (let ((edit-stream-menu (make-instance 'menu :master stream-menu :text "Edit")))
      ;; get levels, then their classes, then their streams as menubuttons with command to edit the stream
      (dolist (level (get-levels))
	(let* ((level-menu (make-instance 'menu :master edit-stream-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let* ((class-menu (make-instance 'menu :master level-menu :text (car class)))
		   (streams (get-streams (cadr class))))
	      (dolist (stream streams)
		(make-instance 'menubutton :master class-menu :text (car stream) :command (lambda () (stream-form (car class) (car stream)))))))))
      )
    ;; add buttons do delete all level to the delete stream menu
    ;; that adds extra complexicity to the datastore because we need versioning to ensure data integrity
    (let ((delete-stream-menu (make-instance 'menu :master stream-menu :text "Delete"))
	  (levels (get-levels)))
      (dolist (level levels)
	(let* ((level-menu (make-instance 'menu :master delete-stream-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let ((streams (get-streams (cadr class)))
		  (class-menu (make-instance 'menu :text (car class) :master level-menu)))
	      (dolist (stream streams)
		(make-instance 'menubutton :master class-menu :text (car stream)
					   :command (lambda ()
						      (let ((message "The stream has been deleted."))
							(handler-case (delete-stream (cadr class) (car stream))
							  (sqlite-constraint-error (err)
							    (declare (ignorable err))
							    (setq message "The stream can't be deleted as it has subjects, first delete them and try again.")))
							(create-menubar)
							(grid-columnconfigure *tk* 0 :weight 1) 
							(grid-rowconfigure *tk* 0 :weight 1)
							(destroy *school-info-main-frame*)
							(setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
							(grid *school-info-main-frame* 0 0)
							(grid (make-instance 'label :master *school-info-main-frame* :text message) 1 0))
						      ))))))))

    ;; subjects
    (let ((new-subject-menu (make-instance 'menu :master subject-menu :text "New"))
	  (levels (get-levels)))
      (dolist (level levels)
	(let* ((level-menu (make-instance 'menu :master new-subject-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let* ((class-menu (make-instance 'menu :master level-menu :text (car class)))
		   (streams (get-streams (cadr class))))
	      (dolist (stream streams)
		(make-instance 'menubutton :master class-menu :text (car stream) :command (lambda () (subject-form stream)))))))))

    (let ((show-subject-menu (make-instance 'menu :master subject-menu :text "Show"))
	  (levels (get-levels)))
      (dolist (level levels)
	(let* ((level-menu (make-instance 'menu :master show-subject-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let* ((class-menu (make-instance 'menu :master level-menu :text (car class)))
		   (streams (get-streams (cadr class))))
	      (dolist (stream streams)
		(make-instance 'menubutton :master class-menu :text (car stream) :command (lambda () (show-stream-subjects stream)))))))))

    (let ((edit-subject-menu (make-instance 'menu :master subject-menu :text "Edit"))
	  (levels (get-levels)))
      (dolist (level levels)
	(let* ((level-menu (make-instance 'menu :master edit-subject-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let* ((class-menu (make-instance 'menu :master level-menu :text (car class)))
		   (streams (get-streams (cadr class))))
	      (dolist (stream streams)
		(let ((subjects (get-stream-subjects (cadr stream)))
		      (streams-menu (make-instance 'menu :master class-menu :text (car stream))))
		  (dolist (subject subjects)
		    (make-instance 'menubutton :master streams-menu :text (cadr subject) :command (lambda () (subject-form stream subject)))))))))))

    (let ((delete-subject-menu (make-instance 'menu :master subject-menu :text "Delete"))
	  (levels (get-levels)))
      (dolist (level levels)
	(let* ((level-menu (make-instance 'menu :master delete-subject-menu :text (car level)))
	       (classes (get-classes (cadr level))))
	  (dolist (class classes)
	    (let* ((class-menu (make-instance 'menu :master level-menu :text (car class)))
		   (streams (get-streams (cadr class))))
	      (dolist (stream streams)
		(let ((subjects (get-stream-subjects (cadr stream)))
		      (streams-menu (make-instance 'menu :master class-menu :text (car stream))))
		  (dolist (subject subjects)
		    (make-instance 'menubutton :master streams-menu :text (cadr subject) :command (lambda ()
												    (let ((message "The subject has been deleted"))
												      (handler-case (delete-subject (car subject))
													(sqlite-constraint-error (err)
													  (declare (ignorable err))
													  (setq message "The subject has papers, first delete them and try again.")))
												      (create-menubar)
												      (grid-columnconfigure *tk* 0 :weight 1) 
												      (grid-rowconfigure *tk* 0 :weight 1)
												      (destroy *school-info-main-frame*)
												      (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
												      (grid *school-info-main-frame* 0 0)
												      (grid (make-instance 'label :master *school-info-main-frame* :text message) 1 0))
												    ))))))))))
    
    ;; houses
    (make-instance 'menubutton :master house-menu :text "New")
    (make-instance 'menubutton :master house-menu :text "Edit")
    (make-instance 'menubutton :master house-menu :text "Delete")

    ;; dormitories
    (make-instance 'menubutton :master dormitory-menu :text "New")
    (make-instance 'menubutton :master dormitory-menu :text "Edit")
    (make-instance 'menubutton :master dormitory-menu :text "Delete")

    ))

(defun start ()
  "start the info application, try to create the tables, bind the error to continue execution if the tables are already present. enable foreign key support on the database"
  (execute-non-query *db* "pragma foreign_keys = on")
  (handler-case
      (create-tables)
    (sqlite-error (err)
      (declare (ignorable err))))
  (with-ltk ()
					; (iconbitmap #p"/home/lam/common-lisp/school/favicon.ico")
    (create-menubar)
    (minsize *tk* 800 600)
    ;; start in maximized on OSX and Windows
    (unless (equal "Linux" (software-type))
      (setf (wm-state *tk*) 'zoomed))
    (wm-title *tk* "School Info")
    ))

(defun level-form (&optional level-text)
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
    (prepare-main-window)
    (grid  level-label 1 0 :padx 10 :pady 5)
    (grid level-entry 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
    (grid save-button 2 2 :pady 10)))

(defun class-form (&optional level-id class-text)
  "collect and process data about classes
   the form has a combobox list of levels to choose from, shows an error if no levels are present."
  (let ((levels (get-levels)))   
    (unless (null *school-info-main-frame*)
      (ltk:destroy *school-info-main-frame*))
    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
    (if levels
	(let* (
	       (level-label (make-instance 'label :master *school-info-main-frame* :text "Select Level"))
	       (level-combobox (make-instance 'combobox :text (caar levels) :master *school-info-main-frame* :values (mapcar (lambda (x) (car x)) levels)))
	       (class-label (make-instance 'label :master *school-info-main-frame* :text "Enter Class Name"))
	       (class-entry (make-instance 'entry :master *school-info-main-frame* :text class-text))
	       (save-button (make-instance 'button :master *school-info-main-frame*
						   :text "Save Class" :command (lambda ()
										 (if class-text
										     (update-class level-id class-text (text class-entry))
										     (save-class (get-level-id (text level-combobox)) (text class-entry)))
										 (create-menubar)
										 (destroy *school-info-main-frame*)
										 (format *standard-output* "~a" (text level-combobox))
										 (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
										 (grid *school-info-main-frame* 0 0)
										 (grid (make-instance 'label :master *school-info-main-frame* :text "The class has been saved.") 1 0)))))
	  (prepare-main-window)
	  (grid level-label 0 0 :padx 10 :pady 5)
	  (grid level-combobox 0 2 :padx 10 :pady 5)
	  (grid class-label 1 0 :padx 10 :pady 5)
	  (grid class-entry 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
	  (grid save-button 2 2 :pady 10))
	(let ((error-text (make-instance 'label :master *school-info-main-frame* :text "There are no levels, first create a level to continue.")))
	  (prepare-main-window)
	  (grid error-text 0 0 :padx 10 :pady 5))
	)))

(defun stream-form (&optional class-text stream-text )
  "collect and process data about streams. includes a combobox for selecting a class, every stream should be part of a class."
  (let ((classes (get-classes)))
    (unless (null *school-info-main-frame*)
      (ltk:destroy *school-info-main-frame*))
    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
    (if classes
	(let* (
	       (class-label (make-instance 'label :master *school-info-main-frame* :text "Choose Class Name"))
	       (class-combobox (make-instance 'combobox :text (caar classes) :master *school-info-main-frame* :values (mapcar (lambda (x) (car x)) classes)))
	       (stream-label (make-instance 'label :master *school-info-main-frame* :text "Enter Stream Name"))
	       (stream-entry (make-instance 'entry :master *school-info-main-frame* :text stream-text))
	       (save-button (make-instance 'button :master *school-info-main-frame*
						   :text "Save Stream" :command (lambda ()
										  (if stream-text
										      (update-stream (get-class-id class-text) stream-text (text stream-entry))
										      (save-stream (get-class-id (text class-combobox)) (text stream-entry)))
										  (create-menubar)
										  (destroy *school-info-main-frame*)
										  (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
										  (grid *school-info-main-frame* 0 0)
										  (grid (make-instance 'label :master *school-info-main-frame* :text "The stream has been saved.") 1 0)))))
	  (prepare-main-window)
	  (grid class-label 1 0 :padx 10 :pady 5)
	  (grid class-combobox 1 2 :padx 10 :pady 5 :sticky "e" :columnspan 5)
	  (grid stream-label 2 0 :padx 10 :pady 5)
	  (grid stream-entry 2 1 :padx 10 :pady 5 :sticky "e" :columnspan 5)
	  (grid save-button 3 2 :pady 10))
	(let ((error-text (make-instance 'label :master *school-info-main-frame* :text "There are no classes, first create a class to add stream to.")))
	  (prepare-main-window)
	  (grid error-text 0 0 :padx 10 :pady 5))
	)))

(defun subject-form (stream-data &optional subject-data)
  "create a new subject; (= stream-data (cons stream stream_id class_id))
   (= subject-data (cons subject_id subject"
  (let ((class (get-class-name (cadr stream-data)))
	(stream (car stream-data))
	(stream-id (cadr stream-data))
	(levels (get-levels))
	(subject-name (cadr subject-data))
	(subject-id (car subject-data)))
    (unless (null *school-info-main-frame*)
      (ltk:destroy *school-info-main-frame*))
    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
    (if levels
	(let* (
	       (class-label (make-instance 'label :master *school-info-main-frame* :text "Class"))
	       (class-entry (make-instance 'entry :master *school-info-main-frame* :text class :state :disabled))
	       (stream-label (make-instance 'label :master *school-info-main-frame* :text "Stream"))
	       (stream-entry (make-instance 'entry :master *school-info-main-frame* :text stream :state :disabled))
	       (subject-label (make-instance 'label :master *school-info-main-frame* :text "Enter Subject"))
	       (subject-entry (make-instance 'entry :master *school-info-main-frame* :text subject-name))
	       (save-button (make-instance 'button :master *school-info-main-frame*
						   :text "Save Subject" :command (lambda ()
										   (if subject-name
										       (update-subject subject-id (text subject-entry))
										       (save-subject stream-id (text subject-entry))
										       )
										   (create-menubar)
										   (destroy *school-info-main-frame*)
										   (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
										   (grid *school-info-main-frame* 0 0)
										   (grid (make-instance 'label :master *school-info-main-frame* :text "The subject has been saved.") 1 0))))
	       )
	  (prepare-main-window)
	  (grid class-label 0 0 :padx 10 :pady 5)
	  (grid class-entry 0 2 :padx 10 :pady 5)
	  (grid stream-label 1 0 :padx 10 :pady 5)
	  (grid stream-entry 1 2 :padx 10 :pady 5)
	  (grid subject-label 2 0 :padx 10 :pady 5)
	  (grid subject-entry 2 2 :padx 10 :pady 5)
	  (grid save-button 3 2 :pady 10))
	(let ((error-text (make-instance 'label :master *school-info-main-frame* :text "There are no levels, first create a level to continue.")))
	  (prepare-main-window)
	  (grid error-text 0 0 :padx 10 :pady 5))
	)))

(defun show-stream-subjects (stream-data)
  "get and display all subjects for a particular stream"
  (let ((stream-subjects (get-stream-subjects (cadr stream-data))))
    (unless (null *school-info-main-frame*)
      (ltk:destroy *school-info-main-frame*))
    (setq *school-info-main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
    (if stream-subjects
	(let* ((subjects-label (make-instance 'label :master *school-info-main-frame* :text (format nil "Subjects for ~a - ~a" (get-class-name (caddr stream-data)) (car stream-data))))
	       (subject-listbox (make-instance 'listbox :master *school-info-main-frame*)))
	  (prepare-main-window)
	  (listbox-append subject-listbox (mapcar #'cadr stream-subjects))
	  (grid subjects-label 0 0 :padx 10 :pady 5)
	  (grid subject-listbox 1 0 :padx 10 :pady 5))
	(let ((error-text (make-instance 'label :master *school-info-main-frame* :text (format nil "There are no subjects for ~a ~a." (cadr stream-data) (car stream-data)))))
	  (prepare-main-window)
	  (grid error-text 0 0 :padx 10 :pady 5))
	)))
