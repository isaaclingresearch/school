(in-package :school.info)

(defvar *test-rows-1* '(("s6" ("sciences" "arts") (("maths" "chemistry" "biology")) (("1" "2") ("1" "2" "3") ("1" "2" "3")))))
(defvar *test-rows* '(("s6" (("sciences" (("maths" ("1" "2")) ("chemistry" ("1" "2" "3")) ("biology" ("1" "2" "3"))))
			     "arts"))))

|# 
STRUCTURE OF THE DATA
The table data is as such: all data is a list. Table data is a list of rows, each of which a list cells, which maybe a string or a list of subcells,
A subcell can be a list of NON-LIST values or a string, consider the car of *test-rows* above s6 is a cell and so is (sciences arts) etc. sciences is 
a subcell but so is (maths chemistry biology), if the a value inside a subcell has multiple values, those are presented as list of subcells in the next position,
forexample: maths has multiple values (1 2) which are presented in the next cell as the first subcell because maths is the first sub-subcell, it will correspond to the
first subcell in the next cell. if this is a bit confusing, look at *test-rows*
#|

(defun replace-nil (list &optional (with 0))
  "replace all nil with 0 or with with if given"
  (if list
      (mapcar (lambda (e) (if (null e) with e)) list)
      with))

(defun cell-heights (row &optional number-of-cells)
  "when given a row, find the number of cells the row will occupy downward (height)"
  (declare (ignore number-of-cells))
  (if (listp row)
      (mapcar (lambda (cell)
		(if (listp cell)
		    `(,(cell-heights (car cell)) ,@(cell-heights (cdr cell)))
		    1))
	      row)
      1))

(defun merge-cell-heights (heights)
  "collect all the heights in a cell in a single list."
  (let* ((r-heights (reverse heights))
	 (acc (list (car r-heights)))
	 (current-data (car acc)))
    (dolist (r-height (cdr r-heights))
      (if (listp r-height)
	  (let (local-acc (local-pos 0))
	    (dolist (height r-height)
	      (if (listp height)
		  (let (sub-local-acc (sub-local-pos local-pos))
		    (dolist (sub-height height)
		      (setq sub-local-acc (append sub-local-acc (list (if (listp current-data)
									  (let ((val (nth sub-local-pos current-data)))
									    (if val val 1))
									  current-data))))
		      (incf sub-local-pos))
		    (setq local-acc (append local-acc (list sub-local-acc)))
		    (setq local-pos sub-local-pos))
		  (progn (setq local-acc (append local-acc (list (if (listp current-data)
								     (let ((val (nth local-pos current-data)))
								       (if val val 1))
								     current-data))))
			 (incf local-pos))))
	    (setq acc (append (list local-acc) acc))
	    (setq current-data local-acc))
	  (setq acc (append (list current-data) acc))))
    acc))

(defun reduce-subcell-heights (heights)
  "add subcell heights"
  (labels ((reduce-cell (cell)
	     (cond ((equal 1 cell) 1)
		   ((summable-listp cell)
		    (mapcar (lambda (data) (reduce #'+ data)) cell))
		   ((not (summable-listp cell))
		    (mapcar #'reduce-cell cell)))))
    (let ((r-heights (reverse heights)))
      (reverse `(,(car r-heights) ,@(mapcar #'reduce-cell  (cdr r-heights)))))))

(defun reduce-cell-heights (heights)
  "add cell heights"
  (labels ((reduce-cell (cell)
	     (cond ((integerp cell) cell)
		   ((summablep cell) (reduce #'+ (remove-if #'null cell))))))
    (let ((reduced-heights (mapcar (lambda (height) (if (listp height) (mapcar #'reduce-cell height) height)) heights)))
      (if (listp (car reduced-heights))
	  `(,(reduce '+ (remove-if #'null (car reduced-heights))) ,@(cdr reduced-heights))
	  reduced-heights))))

(defun get-row-height (row)
  "get the final height of a row"
  (car (reduce-cell-heights (reduce-subcell-heights (merge-cell-heights (cell-heights row))))))

(defun get-cell-heights (row)
  "get the final height of a row"
  (merge-cell-heights (cell-heights row)))

(defun get-row-heights (rows)
  "get the heights of a list of rows"
  (reduce #'+ (mapcar #'get-row-height rows)))

(defun subcell-height (subcell)
  "the length of a subcell is 1 if not a list, or is the length of the list"
  (if (listp subcell)
      (length subcell)
      1))

(defun summable-listp (arg)
  (not (member nil (mapcar #'summablep arg))))

(defun summablep (arg)
  (if (listp arg)
      (not (member t (mapcar (lambda (elem) (not (numberp elem))) arg)))
      nil))

(defun c-zip (list1 list2 &optional acc)
  "adds elements in list2 to elements in list1; length of 1 must be equal or greater than length of 2"
  (cond ((and (null list1) (null list2) acc))
	(t
	 (c-zip (cdr list1) (cdr list2) (append acc (list (append (car list1) (list (car list2)))))))))


;; TODO add data to more than pages.
(defmacro generate-pdf ((&key pre-table (show-page-numbers t) (file-path #P"~/common-lisp/school/mine.pdf")  (type :table) table-data table-title table-headings (table-cell-size 15) (table-padding 6)) &body body)
  "Every page must have the branding of the school, with logo, name, location and contacts,
   then content in the center, the template puts the branding onto the page,
   the file is passed as a file arg, and then the data to be put onto the page is passed as the body
   NOTE: use &body after modifying the macro to work with code.

   the default type is table, which means table data will be provided. you can present some data before the table with pre-table and after the table with body.

   local functions:
   set-position sets the y-position of the page
   draw-list displays a list of data on the page, takes data, an accessor function for each member of the list and spacing as (cons x-position y-spacing)
   draw-table displays data in a table, it is provided with with two args, a list of column headings and a list of lists of data, the function displays data separated by lines, the lines are placed at the end of the widest element in that column.

  see test-table and test-simple-table on how to use the generate-pdf macro
   "
  `(let* ((y-position 841)
	  (x-position 0)
	  (cell-size ,table-cell-size)
	  (cell-padding ,table-padding)
	  (cell-size+padding (+ ,table-cell-size ,table-padding))
	  (1/3-of-padding (/ cell-padding 3))
	  (helvetica (pdf:get-font "Helvetica")))
     (labels ((set-y-position (new-postion) (setf y-position new-postion))
	      (set-x-position (new-postion) (setf x-position new-postion))
	      (draw-table (rows dimensions-data &key other-page)
		(let* ((number-of-rows (get-row-heights rows))
		       (x-start 0) 
		       (y-start 0) ; the y-position at which table starts
		       (x-end 0)
		       (y-end 0)
		       (x-max (apply #'+ dimensions-data))
		       (y-max (* (1+ number-of-rows) cell-size+padding)) ; add one row for the table titles
		       )
		  ;; table title, only show for first page
		  (unless other-page
		    (pdf:in-text-mode
		      (pdf:set-font helvetica 20.0)
		      (set-y-position (- y-position 20)) ; move the y-position 20 below the header line separator
		      (pdf:draw-centered-text 300 y-position ,table-title helvetica 20.0)
		      (set-y-position (- y-position 5)) ; move the y-position 5 below the title, this is the start of the table
		      ))
		  (setq y-start y-position)	      ; see line above
		  
		  
		  ;; center the table, get any remaining space on x, substract the total, divide by 2
		  (when (> 595 x-max)
		    (setq x-start (/ (- 595 x-max) 2)))
		  (setq x-end (+ x-start x-max))
		  (setq y-end (- y-start y-max))
		  
		  ;; upper two Table borders, top most line and line below headings
		  (pdf:move-to x-start y-position)
		  (pdf:line-to x-end y-position)
		  (pdf:stroke)
		  ;; move 15+12 below the starting y-position for the headings, this puts y at the lower separator of the headers
		  (set-y-position (- y-position cell-size+padding))
		  (pdf:move-to x-start y-position)
		  (pdf:line-to x-end y-position)
		  (pdf:stroke)

		  ;; lower border
		  (pdf:move-to x-start y-end)
		  (pdf:line-to x-end y-end)
		  (pdf:stroke)

		  ;; left border
		  (pdf:move-to x-start y-start)
		  (pdf:line-to x-start y-end)
		  (pdf:stroke)

		  ;; right border
		  (pdf:move-to x-end y-start)
		  (pdf:line-to x-end y-end)
		  (pdf:stroke)

		  ;; headings
		  (pdf:set-font helvetica 11.0)
		  (set-x-position x-start) ; start at the beginning of the table
		  ;; set y to the upper border of the table
		  (set-y-position (+ y-position cell-size+padding))
		  ;; we use a closure because headings maybe duplicated, we can't go wrong while counting
		  (let ((pos 0))
		    (dolist (heading ,table-headings)
		      (let* ((row-width (nth pos dimensions-data)))
			(pdf:in-text-mode
			  ;; \<-4-><-text-><-4-><-line->\
			  ;; \<----------width--------->\
			  ;; add the paddding ratios to put some space between the cell text and the separators
			  (pdf:move-text (+ x-position 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
			  (pdf:draw-text heading)
			  (set-x-position (+ x-position row-width))
			  (setq pos (+ 1 pos))))))

		  ;; data
		  (pdf:set-font helvetica 10.0)
		  ;; set y at the lower border of the table headers
		  (set-y-position (- y-position cell-size+padding))
		  ;; iterate over all the data provided
		  (dolist (row rows)
		    (draw-row row dimensions-data x-start y-position))
		  
		  ;; draw row-separators
		  (set-y-position (- y-start ,table-cell-size ,table-padding)) ; account for the headings when drawing row separators
		  (draw-row-separators rows x-start x-end y-position)

		  ;; draw vertical rules to separate the cells
		  (set-x-position x-start) ; start at the beginning of the table
		  (draw-column-separators dimensions-data x-position y-start y-end)))
	      (get-widths (table-headings rows)
		"this function takes headings and data and returns the one with the largest width in an array of length equal to the headings number,
                 for every cell, add 12 for padding."
		(let ((widths (make-sequence 'list (length table-headings) :initial-element 0)))
		  (dolist (heading table-headings)
		    (let ((pos (position heading table-headings :test #'equal)))
		      ;; add the padding to get the total width of a row
		      (setf (nth pos widths) (+ cell-padding (pdf::text-width heading helvetica 10.0)))))
		  (row-widths widths rows 0)))
	      (row-widths (widths rows position)
		"iterate throught the data setting position position to the largest"
		(if (equal position (length widths))
		    widths
		    (let ((local-widths widths))
		      (dolist (row rows)
			(let* ((cell (nth position row))
			       (local-width (nth position widths))
			       (cell-width (cell-width cell local-width)))
			  (when (> cell-width local-width)
			    (setf (nth position local-widths) cell-width))))
		      (row-widths local-widths rows (+ 1 position)))))
	      (cell-width (cell width)
		"get the largest width of one or multiple cell items; add 12 (the padding and line size) to the text size to get the total"
		(if (listp cell)
		    (let ((local-width width))
		      (dolist (cellum cell)
			(let ((new-width (cell-width cellum local-width)))
			  (if (> new-width local-width)
			      (setq local-width new-width))))
		      local-width)
		    (let ((new-width (+ cell-padding (pdf::text-width cell helvetica 10.0))))
		      (if (> new-width width)
			  new-width
			  width))))
	      ;; (draw-list (list-data accessor-function spacing)
	      ;; 	(pdf:in-text-mode
	      ;; 	  (dolist (datum list-data)
	      ;; 	    (set-y-position (- y-position (cdr spacing)))
	      ;; 	    (pdf:move-text (car spacing) y-position)
	      ;; 	    (pdf:draw-text (funcall accessor-function datum)))))
	      (rows-on-front-page ()
		"compute and return how many rows can be put on the front page, this will depend on the cell-size+padding
		we start at 740, move down by 5, then again by cell-size+padding and leave cell-size+padding*2 at the end of the rows, for page numbers and stuff."
		(floor (/ (- 740 5 (* cell-size+padding 3)) cell-size+padding)))
	      (rows-per-page ()
		"compute and return how many rows can be put on a normal page, this will depend on the cell-size+padding
		we start at 841, leave some space up, cell-size+padding*2 and leave cell-size+padding*2 at the end of the rows, for page numbers and stuff."
		(floor (/ (- 841 (* cell-size+padding 4)) cell-size+padding)))
	      (front-page-rows (rows number-of-rows &optional acc (height-acc 0))
		"given a list of rows and the number of rows a frontpage can hold, return a cons (front-page-rows.other-rows)"
		(if rows
		    (let* ((row (car rows))
			   (height (get-row-height row))
			   (new-height-acc (+ height height-acc)))
		      (cond ((= new-height-acc number-of-rows) (cons (append acc (list row)) (cdr rows)))
			    ((> new-height-acc number-of-rows) (cons acc rows))
			    (t (front-page-rows (cdr rows) number-of-rows (append acc (list row)) new-height-acc))))
		    (cons acc rows)))
	      (other-page-rows (rows rows-per-page &optional acc lacc (height-acc 0))
		"returns a list of rows to be put on each page (page-1 page-2 ... page-n)"
		(if rows
		    (let* ((row (car rows))
			   (height (get-row-height row))
			   (new-height-acc (+ height height-acc)))
		      (cond ((= new-height-acc rows-per-page)
			     (other-page-rows (cdr rows) rows-per-page (append acc (list (append lacc (list row))))))
			    ((> new-height-acc rows-per-page) (other-page-rows rows rows-per-page (append acc (list lacc))))
			    (t (other-page-rows (cdr rows) rows-per-page acc (append lacc (list row)) (+ height-acc height)))))
		    acc))	      
	      (draw-row (row dimensions x-start y-start)
		"row is a list of cells, which maybe lists too, we use row height of 15 + 4 left padding + 4 for the line + 4 for the right padding"
		(set-y-position y-start)
		(set-x-position x-start)
		(let ((height-of-row (get-row-height row))
		      (cell-heights (get-cell-heights row))
		      (pos 0)) ;; pos is to track the cell we are at for comparison with other heights
		  (loop for cell in row and cell-height in cell-heights and cell-dimensions in dimensions
			do (draw-cell cell cell-height cell-dimensions x-position y-position)
			   (setq pos (+ 1 pos)))
		  ;; after drawing all cells, move down by the equivalent of the row height to the next row
		  (set-y-position (- y-position (* height-of-row cell-size+padding)))))
	      (draw-cell (cell cell-heights dimensions x-start y-start)
		(if (listp cell)
		    (progn (draw-subcells cell cell-heights dimensions x-start y-start)
			   (set-x-position (+ x-position dimensions)) ;move to next cell
			   )
		    (progn (pdf:in-text-mode
			     ;; the y-start position is the position of the separator above the cell, move 15+4 to the position the text is supposed to be
			     (pdf:move-text (+ x-start (/ cell-padding 3)) (- y-start cell-size 1/3-of-padding)) ; add 4 and 4 for spacing
			     (pdf:draw-text cell))
			   (set-x-position (+ x-position dimensions)) ;move to the next cell
			   )))	      
	      (draw-subcells (subcells heights dimensions x-start y-start)
		"this is a function that takes a list of subcells, draws all of them, at the end, moves to the next cell and at top of the same row (y-start).
                 when the subcell is a list, draw all the subcells with lines separating then move only down."
		(if subcells
		    (if (listp (car subcells))
			(progn (let ((pos 1) ; start at one because we're comapring against length
				     )
				 (loop for cell in (car subcells) and height in (car heights)
				       do (pdf:in-text-mode
					    ;; see above
					    (pdf:move-text (+ x-start 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
					    (pdf:draw-text cell)
					    (pdf:move-to x-start (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding)))   
					    (pdf:line-to (+ x-start dimensions) (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding)))
					    (pdf:stroke)
					    (set-y-position (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding))))
					  (incf pos)))
			       (draw-subcells (cdr subcells) (cdr heights) dimensions x-start y-start))
			(progn (pdf:in-text-mode
				 ;; see draw-cell
				 (pdf:move-text (+ x-start 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
				 (pdf:draw-text (car subcells))
				 (unless (null (cdr subcells))
				   (let* ((height (if (listp (car heights))
						      (reduce #'+ (mapcar (lambda (h) (if (listp h) (reduce #'+ h) h)) (car heights)))
						      (car heights)))
					  (y-stop (- y-position (* height cell-size+padding))))
				     (pdf:move-to x-start y-stop)
				     (pdf:line-to (+ x-start dimensions) y-stop)
				     (pdf:stroke))))
			       (set-y-position (- y-position (* (if (listp (car heights))
								    (reduce #'+ (mapcar (lambda (h) (if (listp h) (reduce #'+ h) h)) (car heights)))
								    (car heights))
								cell-size+padding)))
			       (draw-subcells (cdr subcells) (cdr heights) dimensions x-start y-start)))
		    (set-y-position y-start)))
	      (draw-column-separators (dimensions x-start y-start y-end)
		"draw the vertical lines that separate columns separate the lines from the text by 2"
		;; \<-4-><-text-><-4-><-line->\<-4-><-text-><-4-><-line->\
		;; \<--------dimension------->\<--------dimension------->\
		(cond ((equal (length dimensions) 1))
		      (t (set-x-position (+ x-start (car dimensions))) ; set the position at 4 right of the text in the current cell
			 (pdf:move-to x-position y-start)
			 (pdf:line-to x-position y-end)
			 (pdf:stroke)
			 (draw-column-separators (cdr dimensions) x-position y-start y-end)
			 )))
	      (draw-row-separators (rows x-start x-end y-start)
		"draw separators between the rows except the last one account for the row size (15) and the spacing (12)"
		;; set the y-position to be at y-start
		(set-y-position y-start)
		(let ((pos 1))
		  (dolist (row rows)
		    (let* ((row-len (get-row-height row))
			   (height (* row-len cell-size+padding)))
		      (set-y-position (- y-position height))
		      (pdf:in-text-mode
			(pdf:move-to x-start y-position)
			(pdf:line-to x-end y-position)
			(pdf:stroke))
		      (incf pos))))))
       (let ((dimensions-data (get-widths ,table-headings ,table-data)))
	 (pdf:with-document ()
	   (if (equal ,type :table)
	       ;; iterate over all the data provided
	       (let* ((frontpage-and-other-rows (front-page-rows ,table-data (rows-on-front-page)))
		      (frontpage-rows (car frontpage-and-other-rows))
		      (other-rows (cdr frontpage-and-other-rows))
		      (other-page-rows (other-page-rows other-rows (rows-per-page))))
		 ;; put data on frontpage
		 (pdf:with-page ()
		   (pdf:with-outline-level ("Example" (pdf:register-page-reference))
		     (let* ((school-details (get-school-details))
			    (school-name (cadr (assoc "name" school-details :test #'string-equal)))
			    (pobox (cadr (assoc "pobox" school-details :test #'string-equal)))
			    (email (cadr (assoc "email" school-details :test #'string-equal)))
			    (phone-number (cadr (assoc "phone_number" school-details :test #'string-equal)))
			    (location (cadr (assoc "location" school-details :test #'string-equal)))
			    (logo (make-jpeg-image *logo-path*))
			    )
		       (pdf:add-images-to-page logo)
		       (pdf:draw-image logo 10 750 70 200 0 t)
		       (pdf:in-text-mode
			 (pdf:move-text 100 800)
			 (pdf:draw-centered-text 300 800 school-name helvetica 18.0)
			 (pdf:draw-centered-text 300 785 location helvetica 15.0)
			 (pdf:draw-centered-text 300 775 (format nil "P.O.Box ~a. Email: ~a. Telephone: ~a" pobox email phone-number) helvetica 10.0)
			 (pdf:set-font helvetica 20.0)
			 (pdf:move-text 595 740)
			 (pdf:polyline '((0 740) (595 740))) ; these are the default a4 sizes #(0 0 595 841)
			 (pdf:stroke)
			 (set-y-position 740))
		       ,@pre-table
		       (draw-table frontpage-rows dimensions-data)
		       (when ,show-page-numbers
			 (pdf:in-text-mode
			   (pdf:draw-centered-text 290 cell-size+padding "1" helvetica 10.0)))
		       )))
		 ;; add all other data that is not on the first page
		 (let ((page-count 2))
		   (dolist (rows other-page-rows)
		     (set-y-position (- 841 (* 2 cell-size+padding)))
		     (pdf:with-page ()
		       (pdf:with-outline-level ("Example" (pdf:register-page-reference))
			 (draw-table rows dimensions-data :other-page t))
		       (when ,show-page-numbers
			 (pdf:in-text-mode
			   (pdf:draw-centered-text 290 cell-size+padding (format nil "~d" page-count) helvetica 10.0)))
		       (incf page-count)
		       ))))
	       ,@body)
	   (pdf:write-document ,file-path))))))

(defun test-table ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/table.pdf" :table-title "test" :table-headings '("level" "class" "stream" "grade")
								:table-data '(("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south") ("north" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ("a level" ("s1" "s2" "s3") (("east" "west" "south") ("east" "west" "south")) "A")
									      ))))
(defun test-simple-table ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/simple-table.pdf"
		  :table-title "Levels"
		  :table-headings '("class" "stream" "subject" "papers")
		  :table-data '(("s6" ("sciences" "arts") (("maths" "chemostry" "biology")) (("1" "2") ("1" "2" "3") ("1" "2" "3")))))))
