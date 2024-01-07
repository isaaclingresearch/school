make:
	buildapp --output school-info \
		 --asdf-path . \
	 	 --asdf-tree ~/quicklisp \
	 	 --asdf-tree ~/common-lisp \
	 	 --load-system school \
	 	 --entry school-info:start
