p(defpackage :school.pdf
   (:use :cl :cl-pdf :school :sqlite)
   (:export :generate-pdf :test-table :test-simple-table))

(in-package :school.pdf)

(defparameter *test-rows-1* '(("s6" ("sciences" "arts") (("maths" "chemistry" "biology")) (("1" "2") ("1" "2" "3") ("1" "2" "3")))))
(defparameter *test-rows-with-array* '(("s6" ("sciences" "arts") (("maths" "chemistry" "biology")) (#("1" "2" "3") ("1" "2" "3") ("1" "2" "3")))))
(defparameter *test-rows-with-array-1* '((#("s6" "s5") ("sciences" "arts") (("maths" "chemistry" "biology")) (#("1" "2" "3") ("1" "2" "3") ("1" "2" "3")))))

(defvar *a2-portrait-page-bounds* #(0 0 1191 1684))
(defvar *a2-landscape-page-bounds* #(0 0 1684 1191))
(defvar *a3-portrait-page-bounds* #(0 0 842 1191))
(defvar *a3-landscape-page-bounds* #(0 0 1191 842))
(defvar *a4-page-bounds* #(0 0 595 842))
(defvar *a5-portrait-page-bounds* #(0 0 420 595))
(defvar *a5-landscape-page-bounds* #(0 0 595 420))

(defun get-school-details ()
  (conn (execute-to-list db "select detail_name, detail from school_details")))

|# 
STRUCTURE OF THE DATA
The table data is as such: all data is a list. Table data is a list of rows, each of which a list cells, which maybe a string or a list of subcells,
A subcell can be a list of NON-LIST values or a string, consider the car of *test-rows* above s6 is a cell and so is (sciences arts) etc. sciences is 
a subcell but so is (maths chemistry biology), if the a value inside a subcell has multiple values, those are presented as list of subcells in the next position,
forexample: maths has multiple values (1 2) which are presented in the next cell as the first subcell because maths is the first sub-subcell, it will correspond to the
first subcell in the next cell. if this is a bit confusing, look at *test-rows-1* and *test-rows-with-array*
#|

(defun replace-nil (list &optional (with 0))
  "replace all nil with 0 or with with if given"
  (if list
      (mapcar (lambda (e) (if (null e) with e)) list)
      with))

(defun cell-heights (row &optional number-of-cells)
  "when given a row, find the number of cells the row will occupy downward (height)
   arrays are used to get the represent values on different lines but within the same cell,
   lists are used to represent subcells"
  (declare (ignore number-of-cells))
  (cond ((listp row)
	 (mapcar (lambda (cell)
		   (cond ((listp cell)
			  `(,(cell-heights (car cell)) ,@(cell-heights (cdr cell))))
			 ((and (arrayp cell) (not (stringp cell))) (list (array-total-size cell)))
			 (t 1)))
		 row))
	((and (arrayp row) (not (stringp row))) (list (array-total-size row)))
	(t 1)))

;; this function assumed that the first element is always a one, which was true with lists but not with arrays
(defun merge-cell-heights (heights)
  "collect all the heights in a cell in a single list."
  (let* ((r-heights (reverse (cdr heights)))
	 (acc (list (car r-heights)))
	 (current-data (car acc))
	 (first (car heights)))
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
    (if (equal 1 first)
	(setq acc `(,(car acc) ,@acc))
	;; for all non array, the first value is 1, if it is a list, then the first value is an array
	;; if it is an array, first collect all the other cells, then get the second one's value, sum them and compare
	;; against the value in the list, since it is the length of the array, of less than sum of the total, return the total
	;; otherwise return the total dimensions of the array. subarrays are not affected by the introduction of arrays into the
	;; structure of the data.
	(labels ((reduce-cell (cell)
		   "check if a cell is reducible, or a reducible listp, which it should be either of those or an integer"
		   (cond ((integerp cell) cell)
			 ((summablep cell) (reduce #'+ cell))
			 ((summable-listp cell) (reduce #'+ (mapcar (lambda (subcell) (reduce #'+ subcell)) cell))))))
	  (let ((cdr-sum (if (listp (car acc))
			     (reduce #'+ (mapcar #'reduce-cell (car acc)))
			     (car acc))))
	    (if (> (car first) cdr-sum)
		(setq acc `(,@first ,@acc))
		(setq acc `(,(car acc) ,@acc))))))
    acc))

(defun reduce-subcell-heights (heights)
  "add subcell heights"
  (labels ((reduce-cell (cell)
	     (cond ((equal 1 cell) 1)
		   ((integerp cell) cell)
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
  (when (listp arg)
    (not (member nil (mapcar #'summablep arg)))))

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
(defmacro generate-pdf ((&key pre-table (show-page-numbers t) (file-path #P"~/common-lisp/school/mine.pdf")  (type :table) table-data table-title table-headings (table-cell-size 15) (table-padding 9)) &body body)
  "Every page must have the branding of the school, with logo, name, location and contacts,
   then content in the center, the template puts the branding onto the page,
   the file is passed as a file arg, and then the data to be put onto the page is passed as the body
   NOTE: use &body after modifying the macro to work with code.

   the default type is table, which means table data will be provided. you can present some data before the table with pre-table and after the table with body.

   local functions:
   set-y-position sets the y-position of the page
   set-x-position sets the x-position of the page
   draw-table displays data in a table, it is provided with with two args, a list of column headings and a list of lists of data, the function displays data separated by lines, the lines are placed at the end of the widest element in that column.

  see test functions below on how to use the generate-pdf macro
   "
  `(let* ((y-position (aref *default-page-bounds* 3))
	  (x-position 0)
	  (cell-size ,table-cell-size)
	  (cell-padding ,table-padding)
	  (cell-size+padding (+ ,table-cell-size ,table-padding))
	  (1/3-of-padding (/ cell-padding 3))
	  (helvetica (pdf:get-font "Helvetica")))
     (labels ((set-y-position (new-postion) (setf y-position new-postion))
	      (set-x-position (new-postion) (setf x-position new-postion))
	      (draw-table (rows dimensions-data &key other-page)
		(print dimensions-data)
		(let* ((number-of-rows (get-row-heights rows))
		       (x-start 0) 
		       (y-start 0) ; the y-position at which table starts
		       (page-width (aref *default-page-bounds* 2))
					;       (page-height (aref *default-page-bounds* 3))
		       (x-end 0)
		       (y-end 0)
		       (x-max (apply #'+ dimensions-data))
		       (y-max (* (+ (get-row-height ,table-headings) number-of-rows) cell-size+padding)) ; add the height of the table heaings
		       )
		  ;; table title, only show for first page
		  (unless other-page
		    (pdf:in-text-mode
		      (pdf:set-font helvetica 20.0)
		      (set-y-position (- y-position 20)) ; move the y-position 20 below the header line separator
		      (pdf:draw-centered-text (/ page-width 2) y-position ,table-title helvetica 20.0)
		      (set-y-position (- y-position 5)) ; move the y-position 5 below the title, this is the start of the table
		      ))
		  (setq y-start y-position) ; see line above
		  
		  
		  ;; center the table, get any remaining space on x, substract the total, divide by 2
		  (when (> page-width x-max)
		    (setq x-start (/ (- page-width x-max) 2)))
		  (setq x-end (+ x-start x-max))
		  (setq y-end (- y-start y-max))
		  
		  ;; upper two Table borders, top most line and line below headings
		  (pdf:move-to x-start y-position)
		  (pdf:line-to x-end y-position)
		  (pdf:stroke)
		  ;; get the heights of the heading array and use it to draw the lower separator as headings may occupy more than one line,
		  ;; then multiply that with the cell-size+padding
		  (set-y-position (- y-position (* (get-row-height ,table-headings) cell-size+padding)))
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
		  (set-y-position (+ y-position (* (get-row-height ,table-headings) cell-size+padding)))
		  ;; we use a closure because headings maybe duplicated, we can't go wrong while counting
		  (let ((local-y y-position)) ; local-y will help us start from the upper starting y in case we iterate through a multiline heading
		    (loop for heading in ,table-headings and row-width in dimensions-data
			  do (cond ((and (not (stringp heading)) (arrayp heading))
				    ;; the above clause means we have found a multiline heading,
				    ;; iterate through it and then return to the upper starting point, local-y
				    (loop for i from 0 below (array-total-size heading)
					  do (pdf:in-text-mode (pdf:move-text (+ x-position 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
					       (pdf:draw-text (aref heading i))
					       (print (aref heading i))
					       (set-y-position (- y-position cell-size+padding))))
				    (set-x-position (+ x-position row-width))
				    (set-y-position local-y))
				   (t (pdf:in-text-mode
					;; \<-1/3-><-text-><-1/3-><-line- = 1/3>\
					;; \<----------width------------------->\
					;; add the paddding ratios to put some space between the cell text and the separators
					(pdf:move-text (+ x-position 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
					(pdf:draw-text heading)
					(set-x-position (+ x-position row-width)))))))

		  ;; data
		  (pdf:set-font helvetica 10.0)
		  ;; set y at the lower border of the table headers
		  (set-y-position (- y-position (* (get-row-height ,table-headings) cell-size+padding)))
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
		      ;; the headings are 11 pixels
		      ;; if the heading is an array ie on multiple lines, find the longest of them all
		      (if (and (arrayp heading) (not (stringp heading))) ; strings are arrays but not all arrays are strings
			  (let ((local-width 0))
			    (loop for i from 0 below (array-total-size heading)
				  do (let ((l (+ cell-padding (pdf::text-width (aref heading i) helvetica 11.0))))
				       (when (> l local-width)
					 (setq local-width l))))
			    (setf (nth pos widths) local-width))
			  (setf (nth pos widths) (+ cell-padding (pdf::text-width heading helvetica 11.0))))))
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
		"get the largest width of one or multiple cell items; add cell-padding to the text size to get the total
                 if a cell is an array, iterate through all of the elements to find the longest one"
		(cond ((listp cell) (let ((local-width width))
				      (dolist (cellum cell)
					(let ((new-width (cell-width cellum local-width)))
					  (if (> new-width local-width)
					      (setq local-width new-width))))
				      local-width))
		      (t (cond ((and (arrayp cell) (not (stringp cell)))
				(let ((lw 0))
				  (loop for i from 0 below (array-total-size cell)
					do (let ((cell-width (+ cell-padding (pdf::text-width (aref cell i) helvetica 10.0))))
					     (if (> cell-width lw)
						 (setq lw cell-width))))
				  lw))
			       (t (let ((new-width (+ cell-padding (pdf::text-width cell helvetica 10.0))))
				    (if (> new-width width)
					new-width
					width)))))))
	      (rows-on-front-page ()
		"compute and return how many rows can be put on the front page, this will depend on the cell-size+padding
		we start at (- page-height 101), move down by 5, then again by cell-size+padding and leave cell-size+padding*2 at the end of the rows, for page numbers and stuff."
		(floor (/ (- (aref *default-page-bounds* 3) 101 5 (* cell-size+padding 3)) cell-size+padding)))
	      (rows-per-page ()
		"compute and return how many rows can be put on a normal page, this will depend on the cell-size+padding
		we start at page-height, leave some space up, cell-size+padding*2 and leave cell-size+padding*2 at the end of the rows, for page numbers and stuff."
		(floor (/ (- (aref *default-page-bounds* 3) (* cell-size+padding 4)) cell-size+padding)))
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
		"row is a list of cells, which maybe lists, arrays or string too"
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
		"can be a string, a non string array or a list"
		(cond ((listp cell) (progn (draw-subcells cell cell-heights dimensions x-start y-start)
					   (set-x-position (+ x-position dimensions)) ;move to next cell
					   ))
		      ((and (not (stringp cell)) (arrayp cell))
		       (loop for i from 0 below (array-total-size cell)
			     ;; draw all text below each other, moving down as required
			     do (pdf:in-text-mode
				  (pdf:move-text (+ x-start 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
				  (pdf:draw-text (aref cell i))
				  (set-y-position (- y-position cell-size+padding))))
		       (set-x-position (+ x-position dimensions))
		       (set-y-position y-start) ;move back to the y-start for next cell
		       )
		      (t (progn (pdf:in-text-mode
				  ;; the y-start position is the position of the separator above the cell
				  (pdf:move-text (+ x-start (/ cell-padding 3)) (- y-start cell-size 1/3-of-padding)) ; add 1/3-of-padding for spacing
				  (pdf:draw-text cell))
				(set-x-position (+ x-position dimensions)) ;move to the next cell
				))))	      
	      (draw-subcells (subcells heights dimensions x-start y-start)
		"this is a function that takes a list of subcells, draws all of them, at the end, moves to the next cell and at top of the same row (y-start).
                 when the subcell is a list, draw all the subcells with lines separating then move only down. if the subcell is an array do as above"
		(if subcells
		    (cond ((listp (car subcells))
			   (loop for cell in (car subcells) and height in (car heights)
				 do (pdf:in-text-mode
				      ;; see above
				      (pdf:move-text (+ x-start 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
				      (pdf:draw-text cell)
				      (pdf:move-to x-start (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding)))   
				      (pdf:line-to (+ x-start dimensions) (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding)))
				      (pdf:stroke)
				      (set-y-position (- y-position (* (if (listp height) (reduce #'+ height) height) cell-size+padding)))))
			   (draw-subcells (cdr subcells) (cdr heights) dimensions x-start y-start))
			  ((and (arrayp (car subcells)) (not (stringp (car subcells))))
			   (loop for i from 0 below (array-total-size (car subcells))
				 do (pdf:in-text-mode
				      (pdf:move-text (+ x-start 1/3-of-padding) (- y-position cell-size 1/3-of-padding))
				      (pdf:draw-text (aref (car subcells) i))
				      (set-y-position (- y-position cell-size+padding))))
			   ;; since we move down after every line, just use the y-position as is
			   (pdf:in-text-mode
			     (pdf:move-to x-start y-position)   
			     (pdf:line-to (+ x-start dimensions) y-position)
			     (pdf:stroke))
			   (draw-subcells (cdr subcells) (cdr heights) dimensions x-start y-start))
			  (t (progn (pdf:in-text-mode
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
				    (draw-subcells (cdr subcells) (cdr heights) dimensions x-start y-start))))
		    (set-y-position y-start)))
	      (draw-column-separators (dimensions x-start y-start y-end)
		"draw the vertical lines that separate columns, the dimensions of each are already computed in dimensions"
		(cond ((equal (length dimensions) 1))
		      (t (set-x-position (+ x-start (car dimensions))) 
			 (pdf:move-to x-position y-start)
			 (pdf:line-to x-position y-end)
			 (pdf:stroke)
			 (draw-column-separators (cdr dimensions) x-position y-start y-end)
			 )))
	      (draw-row-separators (rows x-start x-end y-start)
		"draw separators between the rows except the last one, account for the row size and the padding (cell-size+padding)"
		;; set the y-position to be at y-start, also account for the height of the headings, start at the lower border of the headings
		(set-y-position (- y-start (* (get-row-height ,table-headings))))
		(loop for row in (butlast rows)
		      do (let* ((row-len (get-row-height row))
				(height (* row-len cell-size+padding)))
			   (set-y-position (- y-position height))
			   (pdf:in-text-mode
			     (pdf:move-to x-start y-position)
			     (pdf:line-to x-end y-position)
			     (pdf:stroke))))))
       (let* ((dimensions-data (get-widths ,table-headings ,table-data))
	      (x-max (apply #'+ dimensions-data)))
	 (pdf:with-document ()
	   ;; since x-max has already been computed, find if the data can fit on a4 portrait width, then a4 landscape, then try a3 then a2.
	   (cond ((< x-max 595) (setq *default-page-bounds* *a4-portrait-page-bounds*)) 
		 ((< x-max 842) (setq *default-page-bounds* *a3-portrait-page-bounds*))
		 ((< x-max 1191) (setq *default-page-bounds* *a2-portrait-page-bounds*)))
	   (set-y-position (aref *default-page-bounds* 3))
	   (if (equal ,type :table)
	       ;; iterate over all the data provided
	       (let* ((frontpage-and-other-rows (front-page-rows ,table-data (rows-on-front-page)))
		      (frontpage-rows (car frontpage-and-other-rows))
		      (other-rows (cdr frontpage-and-other-rows))
		      (other-page-rows (other-page-rows other-rows (rows-per-page)))
		      (page-width (aref *default-page-bounds* 2))
		      (page-height (aref *default-page-bounds* 3))
		      (page-middle (/ page-width 2)))
		 ;; put data on frontpage
		 (pdf:with-page ()
		   (pdf:with-outline-level ("Example" (pdf:register-page-reference))
		     (let* ((school-details (get-school-details))
			    (school-name (cadr (assoc "name" school-details :test #'string-equal)))
			    (pobox (cadr (assoc "pobox" school-details :test #'string-equal)))
			    (email (cadr (assoc "email" school-details :test #'string-equal)))
			    (phone-number (cadr (assoc "phone_number" school-details :test #'string-equal)))
			    (location (cadr (assoc "location" school-details :test #'string-equal)))
			    (logo (make-jpeg-image (cadr (assoc "logo" school-details :test #'string-equal))))
			    )
		       (pdf:add-images-to-page logo)
		       ;; the logo is 91px long
		       (set-y-position (- y-position 75))
		       (pdf:draw-image logo 10 y-position 70 200 0 t)
		       (set-y-position (- page-height 41))
		       (pdf:in-text-mode
			 (pdf:move-text 100 y-position)
			 (pdf:draw-centered-text page-middle y-position school-name helvetica 18.0)
			 (set-y-position (- y-position 15))
			 (pdf:draw-centered-text page-middle y-position location helvetica 15.0)
			 (set-y-position (- y-position 10))
			 (pdf:draw-centered-text page-middle y-position (format nil "P.O.Box ~a. Email: ~a. Telephone: ~a" pobox email phone-number) helvetica 10.0)
			 (pdf:set-font helvetica 20.0)
			 (set-y-position (- y-position 15))
			 (pdf:move-text page-width y-position)
			 (pdf:polyline (list (list 0 y-position) (list page-width y-position))) ; these are the default a4 sizes #(0 0 595 841)
			 (pdf:stroke))
		       ,@pre-table
		       (draw-table frontpage-rows dimensions-data)
		       (when ,show-page-numbers
			 (pdf:in-text-mode
			   (pdf:draw-centered-text 290 cell-size+padding "1" helvetica 10.0)))
		       )))
		 ;; add all other data that is not on the first page
		 (let ((page-count 2))
		   (dolist (rows other-page-rows)
		     (set-y-position (- page-height (* 2 cell-size+padding)))
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

(defun test-array-table ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/array-table.pdf"
		  :table-title "Levels"
		  :table-headings '("class" "stream" "subject" "papers")
		  :table-data '(("s6" #("sciences" "arts") (("maths" "chemostry" "biology")) (("1" "2") ("1" "2" "3") ("1" "2" "3")))))))

(defun test-simple-array-table ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/s-array-table.pdf"
		  :table-title "Levels"
		  :table-headings '("class" "stream" "subject" "papers")
		  :table-data '(("A level Biology" "A level" #("abdul lam") (#("Chemistry" "Biology")))))))

(defun test-multiline-heading ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/multiline-heading.pdf"
		  :table-title "Levels"
		  :table-headings '(#("class a" "time") "stream" "subject" "papers")
		  :table-data '((#("A level Biology" "b" "c") "A level" #("abdul lam") (#("Chemistry" "Biology")))))))

(defun test-long-table ()
  (generate-pdf  (:file-path #p"~/common-lisp/school/long-table.pdf"
		  :table-title "Levels"
		  :table-headings '("class" "stream" "subject" "papers")
		  :table-data '(("ooooooooooooooooooooooooooooooooooooo" "ooooooooooooooooooooooooooooooooooooo" "ooooooooooooooooooooooooooooooooooooo" "ooooooooooooooooooooooooooooooooooooo")))))
