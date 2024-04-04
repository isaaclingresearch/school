(defpackage :school.teacher
  (:use :cl :sqlite :ltk :cl-pdf :str :school.ltk :school :tktable)
  (:shadow cl-pdf:image cl-pdf:make-image cl-pdf:font-metrics cl-pdf:bbox cl-pdf:name cl-pdf:scale str:repeat)
  (:export :start))

(in-package :school.teacher)

(defparameter *main-frame* nil)		
(defparameter *missing-details-message* "Some school details required to make PDFs are missing, please enter under School Details menu and try again.")
(defparameter *menubar* nil)

(defun create-tables (database)
  (execute-non-query database "create table teacher_info (id integer primary key, surname text, firstname text, date_of_birth text, email text unique, school_email text unique, tel_no1 text unique, tel_no2 text, sex text, marital_status text, number_of_children integer, subject_1 text, subject_2 text, subject_3 text, subject_4 text, teacher_code text unique, residence text, cv text, added_on default current_timestamp)"))

(defun create-or-edit-teacher (surname given-name date-of-birth email school-email tel-no1 tel-no2 sex marital-status number-of-children subject1 subject2 subject3 subject4 teacher-code residence cv &optional id added-on)
  "function to save or edit teacher data"
  (conn
    (if id
	(execute-non-query db "update teacher_info set surname = ?, firstname = ?, date_of_birth = ?, email = ?, school_email = ?, tel_no1 = ?, tel_no2 = ?, sex = ?, marital_status = ?, number_of_children = ?, subject_1 = ?, subject_2 = ?, subject_3 = ?, subject_4 = ?, teacher_code = ?, residence = ?, cv = ?, added_on = ? where id = ?"surname given-name date-of-birth email school-email tel-no1 tel-no2 sex marital-status number-of-children subject1 subject2 subject3 subject4 teacher-code residence cv added-on id)
	(execute-non-query db "insert or into teacher_info (surname, firstname, date_of_birth, email, school_email, tel_no1, tel_no2, sex, marital_status, number_of_children, subject_1, subject_2, subject_3, subject_4, teacher_code, residence, cv) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" surname given-name date-of-birth email school-email tel-no1 tel-no2 sex marital-status number-of-children subject1 subject2 subject3 subject4 teacher-code residence cv))))

(defun get-teacher-info (&optional id)
  "select teacher id, return all values or one corresponding to the given id"
  (conn (if id
	    (execute-to-list db "select * from teacher_info where id = ?" id)
	    (execute-to-list db "select * from teacher_info"))))

(defun start ()
  "start the info application, try to create the tables, bind the error to continue execution if the tables are already present. enable foreign key support on the database"
  (conn
    (execute-non-query db "pragma foreign_keys = on") 
    (handler-case
	(create-tables db)
      (sqlite-error (err)
	(declare (ignore err))))
    )
  (with-ltk ()
    (iconphoto *tk* "~/common-lisp/school/static/logos/teacher.png")
    (create-menubar)
    (minsize *tk* 800 600)
    ;; start in maximized on OSX and Windows
    (unless (equal "Linux" (software-type))
      (setf (wm-state *tk*) 'zoomed))
    (wm-title *tk* "School Teacher")
    ))


(defun prepare-main-frame ()
  (grid *main-frame* 0 0)
  (format-wish "grid anchor . nw")
  (grid-columnconfigure *tk* 0 :weight 1) 
  (grid-rowconfigure *tk* 0 :weight 1))

(defun center-main-frame ()
  (grid *main-frame* 0 0)
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
  (grid (make-instance 'label :master *main-frame* :text message) 1 0)
  )


(defun create-menubar ()
  "create a new menu bar, if an old one exists, destroy it, then recreate a new one."
  (when *menubar*
    (destroy *menubar*))
  (setq *menubar* (make-instance 'menubar))
  (let* ((teachers (make-instance 'menu :master *menubar* :text "Teachers"))
	 (departments (make-instance 'menu :master *menubar* :text "Departments")))
    (declare (ignore departments))
    (teacher-menu teachers)
    ))

(defun teacher-menu (menu)
  "add menu buttons to new teacher menu"
  (make-instance 'menubutton :master menu :text "New Teacher" :command (lambda () (new-teacher-form)))
  (make-instance 'menubutton :master menu :text "Show/Edit Teachers" :command (lambda () (show/edit-teacher-info)))
  (make-instance 'menubutton :master menu :text "Print Teacher Info" :command (lambda () (new-teacher-form)))
  )

(defun new-teacher-form (&optional teacher-id)
  "display form to get new teacher details"
  (unless (null *main-frame*)
    (ltk:destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (wm-title *tk* "Add a new teacher")
  (let ((teacher-info (when teacher-id (car (get-teacher-info teacher-id)))))
    (flet ((get-info-value (pos &optional default)
	     (let ((val (nth pos teacher-info)))
	       (if val val default))))
      (let* ((title (make-instance 'label :master *main-frame* :text "Enter the details of the teacher."))
	     (surname-entry (make-instance 'entry :master *main-frame* :text (get-info-value 1))) ; note that 0 is occupied by id
	     (given-name-entry (make-instance 'entry :master *main-frame* :text (get-info-value 2)))
	     (date-of-birth-entry (make-instance 'entry :master *main-frame* :text (get-info-value 3)))
	     (email-entry (make-instance 'entry :master *main-frame* :text (get-info-value 4)))
	     (school-email-entry (make-instance 'entry :master *main-frame* :text (get-info-value 5)))
	     (phone-number-1-entry (make-instance 'entry :master *main-frame* :text (get-info-value 6)))
	     (phone-number-2-entry (make-instance 'entry :master *main-frame* :text (get-info-value 7)))
	     (sex-entry (make-instance 'entry :master *main-frame* :text (get-info-value 8)))
	     (marital-status-entry (make-instance 'entry :master *main-frame* :text (get-info-value 9)))
	     (number-of-children-entry (make-instance 'entry :master *main-frame* :text (get-info-value 10)))
	     (subject-1-entry (make-instance 'entry :master *main-frame* :text (get-info-value 11)))
	     (subject-2-entry (make-instance 'entry :master *main-frame* :text (get-info-value 12)))
	     (subject-3-entry (make-instance 'entry :master *main-frame* :text (get-info-value 13)))
	     (subject-4-entry (make-instance 'entry :master *main-frame* :text (get-info-value 14)))
	     (residence-entry (make-instance 'entry :master *main-frame* :text (get-info-value 15)))
	     (teacher-code-entry (make-instance 'entry :master *main-frame* :text (get-info-value 16)))
	     (cv-entry (make-instance 'entry :master *main-frame* :text (get-info-value 17)))
	     (button (make-instance 'button :master *main-frame* :text "Save New Teacher"
					    :command (lambda ()
						       (let ((surname (text surname-entry))
							     (given-name (text given-name-entry))
							     (date-of-birth (text date-of-birth-entry))
							     (email (text email-entry))
							     (school-email (text school-email-entry))
							     (phone-number-1 (text phone-number-1-entry))
							     (phone-number-2 (text phone-number-2-entry))
							     (sex (text sex-entry))
							     (marital-status (text marital-status-entry))
							     (number-of-children (text number-of-children-entry))
							     (subject-1 (text subject-1-entry))
							     (subject-2 (text subject-2-entry))
							     (subject-3 (text subject-3-entry))
							     (subject-4 (text subject-4-entry))
							     (residence (text residence-entry))
							     (teacher-code (text teacher-code-entry))
							     (cv (text cv-entry))
							     )
							 (print school-email)
							 (create-or-edit-teacher surname given-name date-of-birth email school-email phone-number-1 phone-number-2 sex marital-status
										 number-of-children subject-1 subject-2 subject-3 subject-4 teacher-code residence cv)
							 (make-response "A new teacher has been added.")
							 ))))
	     )
	(center-main-frame)
	(grid title 0 0)
	(grid (make-instance 'label :master *main-frame* :text "Surname") 1 0)
	(grid surname-entry 1 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Given Name") 2 0)
	(grid given-name-entry 2 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Birthday") 3 0)
	(grid date-of-birth-entry 3 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Email") 4 0)
	(grid email-entry 4 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "School Email") 5 0)
	(grid school-email-entry 5 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Phone number 1") 6 0)
	(grid phone-number-1-entry 6 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Phone number 2") 7 0)
	(grid phone-number-2-entry 7 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Sex") 8 0)
	(grid sex-entry 8 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Marital status") 9 0)
	(grid marital-status-entry 9 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Number of children") 10 0)
	(grid number-of-children-entry 10 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Subject 1") 11 0)
	(grid subject-1-entry 11 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Subject 2") 12 0)
	(grid subject-2-entry 12 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Subject 3") 13 0)
	(grid subject-3-entry 13 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Subject 4") 14 0)
	(grid subject-4-entry 14 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Residence") 15 0)
	(grid residence-entry 15 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "Teacher code") 16 0)
	(grid teacher-code-entry 16 1 :pady 2)
	(grid (make-instance 'label :master *main-frame* :text "CV") 17 0)
	(grid cv-entry 17 1 :pady 2)
	(grid button 18 1 :pady 3)
	))))


(defun tabletest ()
  (with-ltk ()
    (let ((sctable (make-instance 'scrolled-table
				:titlerows 1
				:titlecols 1
				:data
				(cons (cons "*" (loop for c from 1 to 40 collect
						   c))
				      (loop for r from 1 to 200
					 collect
					   (cons r
						 (loop for c from 1 to 40 collect
						      (* r c))))))))
      (pack sctable :side :top :fill :both :expand t)
      (format t "7 * 8 is ~a~%" (car (subvals (table sctable) 7 8)))
      (finish-output))))

(defun show/edit-teacher-info ()
  "display all teacher information in a table"
  (unless (null *main-frame*)
    (ltk:destroy *main-frame*))
  (setq *main-frame* (make-instance 'frame :borderwidth 5 :relief :ridge))
  (prepare-main-frame)
  (wm-title *tk* "Teacher Information Table")
  (let* ((titles '("Id" "Surname" "First name" "Date of birth" "Email" "School Email" "Telephone 1" "Telephone 2" "Sex" "Marital Status" "Children"
		   "Subject 1" "Subject 2" "Subject 3" "Subject 4" "Residence" "Code" "CV" "Added on"))
	 (len (length titles))
	 (teacher-info (get-teacher-info))
	 (info-table (make-instance 'scrolled-table :titlerows 1 :master *main-frame*
						    :data (cons titles teacher-info)
						    )))
    (pack info-table :side :top :fill :both :expand t)
    (grid *main-frame* 0 0 :sticky "nsew")
    (pack (make-instance 'button :master *main-frame* :text "Save"
				 :command (lambda () (let ((acc (group-to-rows (vals (table info-table)) len)))
						       (dolist (teacher (cdr acc)) ; remove the titles from the data as car
							 (eval `(create-or-edit-teacher ,@(butlast (cdr teacher)) ,(car teacher) ,@(last teacher))))
						       (message-box "The teacher information has been updated." "Operation succesful." "ok" "info")
						       )))
	  :after info-table)))

(defun group-to-rows (lst x)
  "Groups a list into sublists containing X elements each."
  (loop for i from 0 below (length lst) by x
        collect (subseq lst i (min (+ i x) (length lst)))))
