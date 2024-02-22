
;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for details of the BSD style license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(in-package :school.info)
(defun row-length (row)
  (reduce #'max (mapcar (lambda (cell) (if (listp cell)
                                           (length cell)
                                           1)) row) :initial-value 1))

(defun count-rows (rows)
  (reduce #'+ (mapcar #'row-length rows)))

(defmacro page-template (&optional (file #P"~/common-lisp/school/mine.pdf") &body body)
  "Every page must have the branding of the school, with logo, name, location and contacts,
   then content in the center, the template puts the branding onto the page,
   the file is passed as a file arg, and then the data to be put onto the page is passed as the body
   NOTE: use &body after modifying the macro to work with code.

   define a gensym to track the progress in the page
   wrap the code in a let, to track the position on the page
   when moving down the page, the size decreases to 0

   local functions:
   set-position sets the y-position of the page
   draw-list displays a list of data on the page, takes data, an accessor function for each member of the list and spacing as (cons x-position y-spacing)
   draw-table displays data in a table, it is provided with with two args, a list of column headings and a list of lists of data, the function displays data separated by lines, the lines are placed at the end of the widest element in that column.
   "
  `(let ((y-position 841)
         (x-position 0)
         (helvetica (pdf:get-font "Helvetica")))
     (labels ((set-y-position (new-postion) (setf y-position new-postion))
              (set-x-position (new-postion) (setf x-position new-postion))
	      (draw-table (table-title table-headings table-data)
                (let* ((dimensions-data (get-widths table-headings table-data))
                       (number-of-rows (count-rows table-data))
                       (x-start 0) 
                       (y-start 0) ; the y-position at which table starts
                       (x-end 0)
                       (y-end 0)
		       ;; \<-4-><-text-><-4-><-line->\ line is also 4
		       ;; \<----------width / height--------->\
                       (line-size+padding (+ 4 4 4)) ;two pixel to each side of the line that is 2 pixels
                       (x-max (apply #'+ dimensions-data))
                       (total-y-line-size (* number-of-rows line-size+padding))
                       (y-max (+ (* number-of-rows 15) total-y-line-size))) ; each row is 15px high, so ,multiply and add spacing

		  ;; table title
                  (pdf:in-text-mode
                    (pdf:set-font helvetica 20.0)
                    (set-y-position (- y-position 20)) ; move the y-position 20 below the header line separator
                    (pdf:draw-centered-text 300 y-position table-title helvetica 20.0)
                    (set-y-position (- y-position 5)) ; move the y-position 5 below the title, this is the start of the table
                    (setq y-start y-position)	      ; see line above
                    )
		  
                  ;; center the table, get any remaining space on x, substract the total, divide by 2
                  (when (> 595 x-max)
                    (setq x-start (/ (- 595 x-max) 2)))
                  (setq x-end (+ x-start x-max))
                  (setq y-end (- y-start y-max 15 12)) ; from y-start, move to max length and then add 10 for the last row to draw the line

		  ;; upper two Table borders, top most line and line below headings
                  (pdf:move-to x-start y-position)
                  (pdf:line-to (- 595 x-start) y-position)
                  (pdf:stroke)
                  (set-y-position (- y-position 15 12)) ; move 15+12 below the starting y-position for the headings
                  (pdf:move-to x-start y-position)
                  (pdf:line-to (- 595 x-start) y-position)
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
		  ;; y-position is constant for the headers, they are on the same line
		  ;; we use a closure because headings maybe duplicated, we can't go wrong while counting
		  (let ((pos 0))
                    (dolist (heading table-headings)
		      (let* ((width (nth pos dimensions-data)))
			(pdf:in-text-mode
			  ;; \<-4-><-text-><-4-><-line->\
			  ;; \<----------width--------->\
                          (pdf:move-text (+ x-position 4) (+ y-position 4)) ; add to put the headings up a bit
                          (pdf:draw-text heading)
                          (set-x-position (+ x-position width))
			  (setq pos (+ 1 pos))))))

                  ;; data
                  (pdf:set-font helvetica 10.0)
                  (setq y-position (- y-start 15 12 15 12)) ; set the y-position at 15 pixels below the headings
                  (dolist (row-data table-data) ; iterate over all the data provided
                    (draw-row row-data dimensions-data x-start y-position))

		  ;; draw row-separators
		  (print y-start)
                  (set-y-position (- y-start 15 12)) ; account for the headings when drawing row separators
                  (draw-row-separators table-data x-start x-end y-position)

		  ;; draw vertical rules to separate the cells
                  (set-x-position x-start) ; start at the beginning of the table
                  (draw-column-separators dimensions-data x-position y-start y-end)))
              (get-widths (table-headings rows)
                "this function takes headings and data and returns the one with the largest width in an array of length equal to the headings number,
                 for every cell, add 12 for padding."
                (let ((widths (make-sequence 'list (length table-headings) :initial-element 0)))
                  (dolist (heading table-headings)
                    (let ((pos (position heading table-headings :test #'equal)))
		      ;; add 12 (the padding) to get the total width of a row
                      (setf (nth pos widths) (+ 12 (pdf::text-width heading helvetica 10.0)))))
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
                    (let ((new-width (+ 12 (pdf::text-width cell helvetica 10.0))))
                      (if (> new-width width)
                          new-width
                          width))))
              (draw-list (list-data accessor-function spacing)
                (pdf:in-text-mode
                  (dolist (datum list-data)
                    (set-y-position (- y-position (cdr spacing)))
                    (pdf:move-text (car spacing) y-position)
                    (pdf:draw-text (funcall accessor-function datum)))))
	      (draw-row (row dimensions x-start y-start)
                "row is a list of cells, which maybe lists too, we use row height of 15 + 2 left padding + 2 for the line + 2 for the right padding"
                (set-y-position y-start)
                (set-x-position x-start)
		(let ((height-of-row (row-length row))
		      (pos 0))
                  (dolist (cell row)
                    (let ((cell-dimensions (nth pos dimensions)))
                      (draw-cell cell cell-dimensions x-position y-position)
		      (setq pos (+ 1 pos))))
		  ;; after drawing all cells, move down by the equivalent of the row height to the next row
		  (set-y-position (- y-position (* height-of-row (+ 15 4 4 4))))))
	      (draw-cell (cell dimensions x-start y-start)
                (if (listp cell)
                    (progn (draw-subcells cell dimensions x-start y-start)
			   (set-x-position (+ x-position dimensions)) ;move to next cell
			   )
                    (progn (pdf:in-text-mode
                             (pdf:move-text (+ 4 x-start) (+ 4 y-start)) ; add 1 and 2 for spacing
                             (pdf:draw-text cell))
                           (set-x-position (+ x-position dimensions)) ;move to the next cell
                           )))	      
              (draw-subcells (subcells dimensions x-start y-start)
		"this is a function that takes a list of subcells, draws all of them, at the end, moves to the next cell and at top of the same row (y-start)."
                (if subcells
                    (progn (pdf:in-text-mode
                             (pdf:move-text (+ 4 x-start) (+ 4 y-position))
                             (pdf:draw-text (car subcells)))
                           (when (cdr subcells)
			     ;; \<-4-><-text-><-4-><-line->\<-4-><-text-><-4-><-line->\
			     ;; \<------dimension--------->\<------dimension--------->\
                             (pdf:move-to x-start y-position)
                             (pdf:line-to (+ x-start dimensions) y-position) ; draw separator to the end of the cell
                             (pdf:stroke))
			   (set-y-position (- y-position 15 12))
                           (draw-subcells (cdr subcells) dimensions x-start y-start))
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
                "draw separators between the rows, when a row has more cells than the number of rows in the table, some cells have multiple values,
                                    account for that. don't draw one for the last one"
                (cond ((equal (length rows) 1))
                      (t (let* ((number-of-cells (row-length (car rows)))
				|# subtruct 1 from the number-of-cells because each row is suppossed to be of height 1, 
                                 if it's not, then add on the height for extra cells, the multiplied value will give a 0 if height is 1, 
                                  so add that before as it is the default, then, if height is more than one, remove 1 because you've already accounted for it
				 \<-4-><-text-><-4-><-line->\<-4-><-text-><-4-><-line->\
				 \<----------dimension----->\<--------------dimension->\ #|
                                (y-end (- y-start 15 12 (* (- number-of-cells 1) (+ 15 4 4 4)))))
        	           (pdf:in-text-mode
                             (pdf:move-to x-start y-end)
                             (pdf:line-to x-end y-end)
                             (pdf:stroke))
                           (draw-row-separators (cdr rows) x-start x-end y-end))
                         ))))
       (pdf:with-document ()
         (pdf:with-page ()
           (pdf:with-outline-level ("Example" (pdf:register-page-reference))
             (let* ((logo (make-jpeg-image "~/common-lisp/school/static/kawanda.jpeg"))
                    (school-details (get-school-details))
                    (school-name (cdr (assoc "name" school-details :test #'string-equal)))
                    (pobox (cdr (assoc "pobox" school-details :test #'string-equal)))
                    (email (cdr (assoc "email" school-details :test #'string-equal)))
                    (phone-number (cdr (assoc "phone_number" school-details :test #'string-equal)))
                    (location (cdr (assoc "location" school-details :test #'string-equal)))
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
               ,@body
               )))
         (pdf:write-document ,file)))))

(defun export-to-pdf (title pdf-path data-function result-function)
  "this is a function to export the data to a pdf, it is provided with a title of the data, pdf-path is the path to save the pdf  and the data-function to call to get the data.
   the result-function to get the data to draw on the pdf" 
  (page-template pdf-path
    (pdf:in-text-mode
      (pdf:set-font helvetica 15.0)
      (set-y-position (- y-position 15))
      (pdf:move-text 50 y-position)
      (pdf:draw-text title)
      (pdf:set-font helvetica 10.0)
      (draw-list (funcall data-function) result-function (cons 100 15)))))

(defun export-table-to-pdf (title pdf-path headings data)
  "this is a function to export the data to a pdf, it is provided with a title of the data, pdf-path is the path to save the pdf  and the data-function to call to get the data.
   the result-function to get the data to draw on the pdf
   example
  (export-table-to-pdf title path '(a b) '((a b) (a (a b)))) 
  if a row has a cell with multiple values, just present them as a list." 
  (page-template pdf-path
    (draw-table title headings data)
    ))

(defun mine-1 (&optional (file #P"~/common-lisp/school/mine.pdf"))
  "the header dividing line is at y-740, all data must be below that."
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let* ((helvetica (pdf:get-font "Helvetica"))
               (logo (make-jpeg-image "~/common-lisp/school/static/kawanda.jpeg"))
               (data (get-school-details))
               (school-name (cdr (assoc "name" data :test #'string-equal)))
               (pobox (cdr (assoc "pobox" data :test #'string-equal)))
               (email (cdr (assoc "email" data :test #'string-equal)))
               (phone-number (cdr (assoc "phone_number" data :test #'string-equal)))
               (location (cdr (assoc "location" data :test #'string-equal)))
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
            )
          )))
    (pdf:write-document file)))

(defun example1 (&optional (file #P"/tmp/ex1.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let ((helvetica (pdf:get-font "Helvetica")))
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 1"))
          (pdf:translate 230 500)
          (loop repeat 150
                for i = 0.67 then (* i 1.045)
                do (pdf:in-text-mode
                     (pdf:set-font helvetica i)
                     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
                     (pdf:move-text (* i 3) 0)
                     (pdf:show-text "cl-typesetting"))
                   (pdf:rotate 13)))))
    (pdf:write-document file)))

;; for the TrueType Example, you need to load the font first:
;; (read the unicode-readme for more info)
#+nil
(pdf:load-ttu-font #P"/tmp/times.ufm" #P"/tmp/times.ttf")

(defun example1-ttu (&optional (file #P"/tmp/ex1-ttu.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let ((helvetica (pdf:get-font "TimesNewRomanPSMT"))) ; The windows times font
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 1 with Unicode"))
          (pdf:translate 230 500)
          (loop repeat 150
                for i = 0.67 then (* i 1.05)
                do (pdf:in-text-mode
                     (pdf:set-font helvetica i)
                     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
                     (pdf:move-text (* i 3) 0)
                     (pdf:show-text (format nil "Lisp lives! ~cx.~cy.x " (code-char 955)(code-char 955))))
                   (pdf:rotate 13)))))
    (pdf:write-document file)))

(defun example2 (&optional (file #P"/tmp/ex2.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let ((helvetica (pdf:get-font "Helvetica")))
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 2"))
          (pdf:move-to (+ 10 (random 500))(+ 10 (random 400)))
          (pdf:set-gray-fill 0.5)
          (dotimes (i 50)
            (pdf:line-to (+ 50 (random 500)) (+ 50 (random 400))))
          (pdf:close-even-odd-fill-and-stroke)
          (pdf:move-to (+ 50 (random 500))(+ 400 (random 400)))
          (pdf:set-rgb-fill 0.5 0.5 0.8)
          (pdf:set-rgb-stroke 0.9 0.5 0.1)
          (dotimes (i 50)
            (pdf:bezier2-to (+ 50 (random 500)) (+ 400 (random 400))
                            (+ 50 (random 500)) (+ 400 (random 400))))
          (pdf:close-even-odd-fill-and-stroke))))
    (pdf:write-document file)))

(defun gen-image-bits ()
  (with-output-to-string (s)
    (loop for x from -10 to 10 by 1/10
          do (loop for y from -10 to 10 by 1/10
                   do (format s "~2,'0x~2,'0x~2,'0x"
                              (round (+ 200 (* 55 (sin x))))
                              (round (+ 200 (* 55 (cos y))))
                              (round (+ 200 (* 55 (sin (+ x y))))))))))

(defun example3 (&optional (file #P"/tmp/ex3.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let* ((helvetica (pdf:get-font "Helvetica"))
               (image (make-instance 'pdf:image
                                     :bits (gen-image-bits)
                                     :width 201 :height 201)))
          (pdf:draw-bar-code128 "30A0033111436" 20 100)
          (pdf:add-images-to-page image)
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 3"))
          (pdf:with-saved-state
            (pdf:translate 102 550)
            (pdf:rotate 20)
            (pdf:scale 200 125)
            (pdf:paint-image image))
          (pdf:with-saved-state
            (pdf:translate 100 540)
            (pdf:rotate -70)
            (pdf:scale 300 200)
            (pdf:paint-image image)))))
    (pdf:write-document file)))


;; logo

(defparameter *fractal-ratio* 0.8)
(defconstant +sin60+ (sin (/ pi 3)))
(defconstant +cos60+ (cos (/ pi 3)))
(defconstant +tg30+ (tan (/ pi 6)))
(defconstant +tg60-tg30+ (- (tan (/ pi 3))(tan (/ pi 6))))

(defun %fractal (x y dx dy level)
  (if (zerop level)
      (let ((dx/2 (* dx 0.5))
            (dy/2 (* dy 0.5)))
        (pdf:move-to (- x dx/2) (- y dy/2))
        (pdf:line-to x (+ y dy/2))
        (pdf:line-to (+ x dx/2) (- y dy/2))
        (pdf:close-fill-and-stroke))
      (let* ((delta (- 1 *fractal-ratio*))
             (delta05 (* 0.5 delta))
             (ratio2 (- 1 delta05))
             (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
             (deltay (* dy 0.25 (+ 1 delta05)))
             (dyf2 (* dy 0.5 (+ 1 delta05 )))
             (dxf2 (* dx 0.5 (+ 1 delta05 ))))
        (decf level)
        (setf dx (* dx 0.5))
        (setf dy (* dy 0.5))
        (%down-fractal x (- y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
        (%fractal x      (+ y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        (%fractal (+ x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        (%fractal (- x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        )))

(defun %down-fractal (x y dx dy level)
  (setf level 0)
  (if (zerop level)
      (let ((dx/2 (* dx 0.5))
            (dy/2 (* dy 0.5)))
        (pdf:move-to (- x dx/2) (+ y dy/2))
        (pdf:line-to x (- y dy/2))
        (pdf:line-to (+ x dx/2)(+ y dy/2))
        (pdf:close-fill-and-stroke))
      (let* ((delta (- 1 *fractal-ratio*))
             (delta05 (* 0.5 delta))
             (ratio2 (- 1 delta05))
             (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
             (deltay (* dy 0.25 (+ 1 delta05)))
             (dyf2 (* dy 0.5 (+ 1 delta05 )))
             (dxf2 (* dx 0.5 (+ 1 delta05 ))))
        (decf level)
        (setf dx (* dx 0.5))
        (setf dy (* dy 0.5))
        (%fractal x (+ y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
        (%down-fractal x      (- y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        (%down-fractal (+ x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        (%down-fractal (- x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
        )
      ))

(defun fractal (x y l level)
  (let ((dx l)
        (dy (* l +sin60+)))
    (%fractal x y dx dy level)))

					;the logo
(defun example4 (&optional (file #P"/tmp/ex4.pdf"))
  (pdf:with-document ()
    (loop for i from 1 to 7
          do (pdf:with-page ()
               (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
                 (let* ((helvetica (pdf:get-font "Helvetica")))
                   (pdf:in-text-mode
                     (pdf:set-font helvetica 36.0)
                     (pdf:move-text 100 800)
                     (pdf:draw-text (format nil "cl-pdf: Example 4    page ~d" i)))
                   (pdf:set-rgb-stroke 1.0 1.0 1.0)
                   (pdf:set-rgb-fill 0.4 0.4 0.9)
                   (pdf:set-line-width 0.2)
                   (fractal 298 530 600 i)))))
    (pdf:write-document file)))

(defvar *dx* #(1 0 -1 0))
(defvar *dy* #(0 1 0 -1))

					;make-maze
(defun example5 (&key (nx 100) (ny 150) (size 5) (file #P"/tmp/ex5.pdf"))
  (let ((x-stack '())
        (y-stack '())
        (visited (make-array (list nx ny) :initial-element nil))
        (v-walls (make-array (list nx ny) :initial-element t))
        (h-walls (make-array (list nx ny) :initial-element t))
        (x (random nx))
        (y (random ny))
        next-x next-y)
    (flet ((find-cell ()
             (let ((tested (vector nil nil nil nil))
                   (nb-tested 0))
               (loop while (< nb-tested 4)
                     for test = (random 4)
                     unless (svref tested test)
                       do (incf nb-tested)
			  (setf (svref tested test) t)
			  (setf next-x (+ x (svref *dx* test)))
			  (setf next-y (+ y (svref *dy* test)))
			  (when (and (>= next-x 0)(< next-x nx)(>= next-y 0)(< next-y ny)
                                     (not (aref visited next-x next-y)))
			    (return-from find-cell t)))
               nil)))
      (setf (aref visited x y) t)
      (loop with nb-visited = 1 and total-cells = (* nx ny)
            while (< nb-visited total-cells)
            do (if (find-cell)
                   (progn (push x x-stack)(push y y-stack)
                          (if (/= next-x x)
                              (setf (aref h-walls (min x next-x) y) nil)
                              (setf (aref v-walls x (min y next-y)) nil))
                          (setf x next-x y next-y)
                          (setf (aref visited x y) t)
                          (incf nb-visited))
                   (progn (setf x (pop x-stack) y (pop y-stack))))))
    (pdf:with-document ()
      (pdf:with-page ()
        (pdf:with-outline-level ("Example" (pdf:register-page-reference))
          (pdf:translate (* 0.5 (- 595 (* nx size)))(* 0.5 (- 841 (* ny size))))
          (setf (aref h-walls (1- nx) (random ny)) nil)
          (pdf:move-to 0 0)
          (pdf:line-to (*  nx size) 0)
          (pdf:move-to 0 size)
          (pdf:line-to 0 (* ny size))
          (loop for x from 0 below nx
                for x0 = 0 then x1
                for x1 from size by size
                do (loop for y from 0 below ny
                         for y0 = 0 then y1
                         for y1 from size by size
                         do
                            (when (aref h-walls x y)
                              (pdf:move-to x1 y0)
                              (pdf:line-to x1 y1))
                            (when (aref v-walls x y)
                              (pdf:move-to x0 y1)
                              (pdf:line-to x1 y1)))
                   (pdf:stroke))))
      (pdf:write-document file))))


(defun example6 (&optional (file #P"/tmp/ex6.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let ((helvetica (pdf:get-font "Helvetica")))
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 6"))
          (pdf:set-rgb-stroke 0.1 0.1 0.1)
          (pdf:set-rgb-fill 0.8 0.8 0.8)
          (let ((x 50) (y 600))
            (dotimes (i 2)
              (pdf:rectangle x y 500 140 :radius 10)
              (pdf:close-fill-and-stroke)
              (setf y (- y 180))))
          (pdf:translate 50 670)
          (let ((x 50) (y 0))
            (loop repeat 4
                  for i = 8 then (* i 1.05)
                  do
                     (pdf:set-rgb-fill (* 0.1 i) (* 0.01 i) (* 0.02 i))
                     (pdf:circle x y (* 4 i))
                     (pdf:close-fill-and-stroke)
                     (pdf:ellipse (+ x 250) y (* 5 i) (* 4 i))
                     (pdf:close-fill-and-stroke)
                     (setf x (+ x 50))))
          (pdf:translate 0 -180)
          (pdf:regular-polygon 150 0 50 7 :fillet-radius 8)
          (pdf:close-fill-and-stroke)
          (pdf:star 350 0 50 30 6 :fillet-radius 5)
          (pdf:close-fill-and-stroke)

          (pdf:set-rgb-fill 0.8 0.6 0.2)
          (pdf:regular-polygon 150 0 30 5 :fillet-radius 4)
          (pdf:close-fill-and-stroke)
          (pdf:star 350 0 40 20 4 :fillet-radius 6)
          (pdf:close-fill-and-stroke)

          (pdf:set-rgb-fill 0.4 0.8 0.7)
          (pdf:regular-polygon 150 0 15 3 :fillet-radius 3)
          (pdf:close-fill-and-stroke)
          (pdf:star 350 0 35 10 12 :fillet-radius 1)
          (pdf:close-fill-and-stroke)
          (pdf:set-line-width 0.5)
          (loop for r from 2 to 100 by 2
                for start = (* pi 0.001 (random 2000))
                for length = (* pi 0.001 (random 2000))
                do (pdf:set-rgb-stroke (* 0.01 (random 100))(* 0.01 (random 100))(* 0.01 (random 100)))
                   (pdf:arc 250 -230 r start length)
                   (pdf:stroke)))))
    (pdf:write-document file)))

(defvar *test-jpeg-file-path* (when *load-pathname*
                                (merge-pathnames #P"banner.jpg" *load-pathname*)))

(unless *test-jpeg-file-path*
  (error "please set the *test-jpeg-file-path* variable to the banner.jpg file location"))

(defvar *test-jpeg* *test-jpeg-file-path*)

(defun example7 (&optional (file #P"/tmp/ex7.pdf"))
  (pdf:with-document ()
    (let ((jpg-image (pdf:make-jpeg-image *test-jpeg*))
          (helvetica (pdf:get-font "Helvetica")))
      (pdf:with-outline-level ("Contents" "page 1")
        (pdf:with-page ()
          (pdf:register-page-reference "page 1")
          (pdf:with-outline-level ("Page 1" "page 1")
            (pdf:in-text-mode
              (pdf:set-font helvetica 36.0)
              (pdf:move-text 100 800)
              (pdf:draw-text "cl-pdf: Example 7"))
            (pdf:set-rgb-stroke 0.1 0.1 0.1)
            (pdf:set-rgb-fill 0.6 0.6 0.8)
            (pdf:in-text-mode
              (pdf:set-font helvetica 13.0)
              (pdf:move-text 10 700)
              (pdf:draw-text "Test for bookmarks, JPEG support, internal links, URI links and basic charts"))
            (pdf:add-images-to-page jpg-image)
            (pdf:draw-image jpg-image 10 10 239 50 0 t)
            (pdf:add-URI-link 10 10 239 50 "http://www.fractalconcept.com/asp/html/cl-pdf.html" :border #(1 1 1))
            (pdf:in-text-mode
              (pdf:set-font helvetica 10.0)
              (pdf:move-text 500 10)
              (pdf:draw-text "goto page 2"))
            (pdf:add-link 495 8 80 14 "page 2")
            (pdf:draw-object (make-instance 'pdf:histogram :x 200 :y 450 :width 300 :height 200
                                                           :label-names '("Winter" "Spring" "Summer" "Autumn")
                                                           :labels&colors '(("Serie 1" (1.0 0.0 0.0))
                                                                            ("Serie 2" (0.0 1.0 0.0)))
                                                           :series '((42 46 48 42)(40 38 51 46))
                                                           :background-color '(0.9 0.9 0.9)
                                                           :stacked-series nil ;;; try also with t
                                                           :x-axis-options ()
                                                           :y-axis-options ()
                                                           :legend-options ()))
            (pdf:draw-object (make-instance 'pdf:pie-chart :x 200 :y 100 :width 200 :height 200
                                                           :serie '(12 23 65 33)
                                                           :labels&colors '(("Winter" (1.0 0.0 0.0))
                                                                            ("Spring" (0.0 1.0 0.0))
                                                                            ("Summer" (0.0 0.0 1.0))
                                                                            ("Autumn" (0.0 1.0 1.0)))))))
        (pdf:with-page ()
          (pdf:register-page-reference "page 2")
          (pdf:with-outline-level ("Page 2" "page 2")
            (pdf:in-text-mode
              (pdf:set-font helvetica 36.0)
              (pdf:move-text 100 800)
              (pdf:draw-text "Page 2"))
            (pdf:add-images-to-page jpg-image)
            (pdf:draw-image jpg-image 10 10 239 50 0 t)
            (pdf:add-URI-link 10 10 239 50 "http://www.fractalconcept.com/asp/html/cl-pdf.html" :border #(1 1 1))
            (pdf:in-text-mode
              (pdf:set-font helvetica 10.0)
              (pdf:move-text 500 10)
              (pdf:draw-text "goto page 1"))
            (pdf:add-link 495 8 80 14 "page 1")
            (pdf:draw-object
             (make-instance 'pdf:plot-xy :x 100 :y 400 :width 400 :height 200
                                         :labels&colors '(("Data 1" (1.0 0.0 0.0))
                                                          ("Data 2" (0.0 1.0 0.0))
                                                          ("Data 3" (0.0 0.0 1.0)))
                                         :series '(((1 40) (3 38) (5 31) (7 36))
                                                   ((2 53) (2.5 42) (3.7 46) (6 48))
                                                   ((1.3 12) (1.6 18) (2 16) (3 27)))
                                         :background-color '(0.9 0.9 0.9)
                                         :x-axis-options ()
                                         :y-axis-options '(:min-value 0)
                                         :legend-options ()))))))
    (pdf:write-document file)))

					; Von Koch fractal (brute force ;-))

(defun vk-fractal (l level)
  (pdf:with-saved-state
    (if (zerop level)
        (progn
          (pdf:move-to 0 0)
          (pdf:line-to l 0)
          (pdf:stroke))
        (loop with l3 = (/ l 3.0) and l-1 = (1- level)
              for angle in '(nil 60 -120 60)
              do (when angle (pdf:rotate angle))
		 (vk-fractal l3 l-1))))
  (pdf:translate l 0))


(defun example8 (&optional (file #P"/tmp/ex8.pdf"))
  (pdf:with-document ()
    (loop for i from 0 to 6
          do (pdf:with-page ()
               (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
                 (let* ((helvetica (pdf:get-font "Helvetica" :win-ansi-encoding)))
                   (pdf:draw-centered-text 297 800
                                           (format nil "Koch's flake (Level ~d, ~d segments, perimeter ~,1f mm)"
                                                   i (* 3 (expt 4 i))(/ (* 180 (* 3 (expt 4 i)))(expt 3 i)))
                                           helvetica 18.0)
                   (pdf:translate 42 530)
                   (pdf:set-line-width 0.1)
                   (vk-fractal 510 i)(pdf:rotate -120)(vk-fractal 510 i)(pdf:rotate -120)(vk-fractal 510 i)))))
    (pdf:write-document file)))

;;; A Mandelbrot set from Yannick Gingras

(defun hsv->rgb (h s v)
  "channels are in range [0..1]"
  (if (eql 0 s)
      (list v v v)
      (let* ((i (floor (* h 6)))
             (f (- (* h 6) i))
             (p (* v (- 1 s)))
             (q (* v (- 1 (* s f))))
             (t_ (* v (- 1 (* s (- 1 f)))))
             (hint (mod i 6)))
        (case hint
          (0 (list v t_ p))
          (1 (list q v p))
          (2 (list p v t_))
          (3 (list p q v))
          (4 (list t_ p v))
          (5 (list v p q))))))

(defun make-color-map (nb-col
                       start-rad
                       &optional
                         stop-rad
                         (sat .85)
                         (tilt-angle 0)
                         (nb-loop 1)
                         (clockwise t))
  ;; borowed from Poly-pen --YGingras
  (let* ((stop-rad (if stop-rad stop-rad start-rad))
         (angle-inc (* (if clockwise 1.0 -1.0) nb-loop))
         (val-inc (- stop-rad start-rad)))
    (coerce (loop for k from 0 to (1- nb-col) collect
					      (let ((i (/ k (1- nb-col))))
						(mapcar #'(lambda (x) (round x 1/255))
							(hsv->rgb (mod (+ tilt-angle (* i angle-inc)) 1)
								  sat
								  (+ start-rad (* i val-inc))))))
            'vector)))


(defun gen-mandelbrot-bits (w h)
  ;; Inside a Moth by Lahvak, for other interesting regions see
  ;;     http://fract.ygingras.net/top

  ;; TODO:AA
  (declare (optimize speed (debug 0) (safety 0) (space 0))
           (type fixnum w h))
  (let* ((nb-cols 30000)
         (nb-iter (expt 2 11)) ;; crank this if you have a fast box
         (center #c(-0.7714390420105d0 0.1264514778485d0))
         (zoom (* (expt (/ h 320) 2) 268436766))
         (inc (/ h (* 150000d0 zoom)))
         (cols (make-color-map nb-cols 1 0.2 0.9 0.24 0.21 t))
         (c #c(0d0 0d0))
         (z #c(0d0 0d0))
         (region nil))
    (declare (type double-float inc))
    (dotimes (i h)
      (dotimes (j w)
        (setf c (complex (+ (realpart center)
                            (* inc (+ (the fixnum j) (/ w -2d0))))
                         (+ (imagpart center)
                            (* inc (+ (the fixnum i) (/ h -2d0)))))
              z #c(0d0 0d0))
        ;; standard Mandelbrot Set formula
        (push (dotimes (n nb-iter 0)
                (setf z (+ (* z z) c))
                (when (< 2 (abs z))
                  (return (- nb-iter
                             ;; sub-iter smoothing
                             (- n (log (log (abs (+ (* z z) c)) 10) 2))))))
              region)))
    (with-output-to-string (s)
      (let ((max (reduce #'max region)))
        (dolist (x (nreverse region))
          (destructuring-bind (r g b)
              (if (zerop x)
                  '(0 0 0)
                  ;; pallette stretching
                  (elt cols (floor (expt (/ x max) (/ nb-iter 256))
                                   (/ 1 (1- nb-cols)))))
            (format s "~2,'0x~2,'0x~2,'0x" r g b)))))))

;;; Example 9 is a Mandelbrot set from Yannick Gingras
;;; Takes a long long time...

(defun example9 (&optional (file #P"/tmp/ex9.pdf"))
  "draw a nice region of the Mandelbrot Set"
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
        (let* ((w 600)
               (h 750)
               (helvetica (pdf:get-font "Helvetica"))
               (image (make-instance 'pdf:image
                                     :bits (gen-mandelbrot-bits w h)
                                     :width w :height h)))
          (pdf:add-images-to-page image)
          (pdf:in-text-mode
            (pdf:set-font helvetica 36.0)
            (pdf:move-text 100 800)
            (pdf:draw-text "cl-pdf: Example 9"))
          (pdf:with-saved-state
            (pdf:translate 0 0)
            (pdf:scale (/ w 2) (/ h 2))
            (pdf:paint-image image)))))
    (pdf:write-document file)))

;;; Example 10: trnasparency by Eric Marsden

(defun example10 (&optional (file #p"/tmp/ex10.pdf"))
  (pdf:with-document ()
    (dolist (bm '(:normal :multiple :screen :overlay :darken :lighten :ColorDodge :ColorBurn
                  :HardLight :SoftLight :difference :exclusion :saturation :color :luminosity))
      (let ((helvetica (pdf:get-font "Helvetica")))
        (pdf:with-page ()
          (pdf:with-outline-level ((format nil "Blend mode ~A" bm) (pdf:register-page-reference))
            (pdf:in-text-mode
              (pdf:set-font helvetica 14.0)
              (pdf:move-text 15 820)
              (pdf:draw-text (format nil "PDF transparency with ~A blend-mode" bm)))
            (dotimes (y 10)
              (pdf:in-text-mode
                (pdf:set-font helvetica 10.0)
                (pdf:set-rgb-fill 0 0 0)
                (pdf:move-text 10 (+ 45 (* y 80)))
                (pdf:draw-text (format nil "Alpha = ~,1F" (/ y 9.0))))
              (pdf:set-rgb-fill 1 0.6 0.2)
              (pdf:rectangle 70 (+ 20 (* y 80)) 500 30)
              (pdf:fill-path))
            (pdf:translate 100 50)
            (dotimes (y 10)
              (dotimes (x 10)
                (apply #'pdf:set-rgb-fill (hsv->rgb (/ x 9.1) 1 1))
                (pdf:set-transparency (/ y 9.0) bm)
                (pdf:circle (* x 50) (* y 80) 30)
                (pdf:fill-path)))))))
    (pdf:write-document file)))

;;; Example 11: lazy image loading
(defun example11 (&optional (file #p"/tmp/ex11.pdf"))
  (let ((pdf:*load-images-lazily* t))
    (pdf:with-document ()
      (loop for f in (directory (merge-pathnames "*.png"))
            do (pdf:with-page ()
                 (let ((jpg (pdf:make-image f)))
                   (setf (pdf:bounds pdf:*page*) (vector 0 0 (pdf:width jpg) (pdf:height jpg)))
                   (pdf:add-images-to-page jpg)
                   (pdf:draw-image jpg 0 0 (pdf:width jpg) (pdf:height jpg) 0 t))))
      (pdf:write-document file))))
