;; the application manager of the school suit of applications.
;; all applications will have dedicated windows and have processes as children of the main process.
;; all applications will share a single database and main window.
;; we shall add applications to the suite with time.
;; designed with ltk and sqlite3 as the database.
;; special thanks to the creators of ltk and sqlite and those of sbcl and those of lisp.
;; we will use the power for good.

(defsystem "school"
  :author "Ninx technology limited"
  :description "The application manager for the school application"
  :depends-on (:ltk :sqlite :cl-pdf :cl-pdf-parser :str)
  :components ((:file "info")
	       (:file "school")
	       (:file "pdf"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "school-info"
  :entry-point "school-info:start")
