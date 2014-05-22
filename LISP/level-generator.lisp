
(defun generate-level (height width first)
	(defparameter *height* (- height 1))
	(defparameter *width* (- width 1))
	(initialize-level)
	(defparameter *rooms* (list (init-room)))
	(loop for i from 1 to (+ 7 (random 7))
		do
		(connect (elt *rooms* (random (length *rooms*))) (init-room))
		)
	(add-walls)

	(place-in-room (elt *rooms* (random (length *rooms*))) #\<)
	(place-in-room (elt *rooms* (random (length *rooms*))) #\>)

	(loop for room in *rooms* do
		(if (= (random 4) 0) (place-in-room room #\g))
		(if (= (random 7) 0) (place-in-room room #\%))
		)

	(if first (place-in-room (elt *rooms* (random (length *rooms*))) #\@))

	(2d-vector-to-string *fresh-level*)
	)

(defun initialize-level ()
	(defparameter *fresh-level* (make-array (+ *height* 5) :fill-pointer 0))
	(loop for i from 0 to *height* 
		do
		(vector-push (make-array (+ *width* 5) :fill-pointer 0) *fresh-level*)
		(loop for j from 0 to *width* 
			do
			(vector-push #\Space (elt *fresh-level* i)))
		)
	)

(defclass chamber ()
	((height
		:initarg :height)
	(width
		:initarg :width)
	(y-pos
		:initarg :y-pos)
	(x-pos
		:initarg :x-pos) 
	(connected :initform nil)
	)
)

(defun init-room ()
	(defparameter *new-room* (make-instance 'chamber))
	(setf (slot-value *new-room* 'height) (+ (random 4) 2))
	(setf (slot-value *new-room* 'width) (+ (random 4) 2))
	(setf (slot-value *new-room* 'y-pos) (random (- *height* (slot-value *new-room* 'height))))
	(setf (slot-value *new-room* 'x-pos) (random (- *width* (slot-value *new-room* 'width))))
	(loop for y from (slot-value *new-room* 'y-pos) to (+ (slot-value *new-room* 'y-pos) (- (slot-value *new-room* 'height) 1))
		do
		(loop for x from (slot-value *new-room* 'x-pos) to (+ (slot-value *new-room* 'x-pos) (- (slot-value *new-room* 'width) 1) )
			do
			(setf (elt (elt *fresh-level* y) x) #\.)
			)
		)
	*new-room*
	)

(defun connect (connected-room new-room)
	(defparameter *y-new* (+ (slot-value *new-room* 'y-pos) (random (slot-value new-room 'height))))
	(defparameter *x-new* (+ (slot-value *new-room* 'x-pos) (random (slot-value new-room 'width))))
	(defparameter *y-old* (+ (slot-value connected-room 'y-pos) (random (slot-value new-room 'height))))
	(defparameter *x-old* (+ (slot-value connected-room 'x-pos) (random (slot-value new-room 'width))))
	
	(if (> *x-old* *x-new*)
		(x-corridor *x-new* *x-old* *y-new*)
		(x-corridor *x-old* *x-new* *y-new*)
		)
	(if (> *y-old* *y-new*)
		(y-corridor *y-new* *y-old* *x-old*)
		(y-corridor *y-old* *y-new* *x-old*)
		)
	(setf *rooms* (append *rooms* (list *new-room*)))
)

(defun place-in-room (room symbol)
	(defvar success nil)
	(loop
		do
		(defparameter *x* (+ (slot-value room 'x-pos) (random (slot-value room 'width))))
		(defparameter *y* (+ (slot-value room 'y-pos) (random (slot-value room 'height))))
		(when (char= (elt (elt *fresh-level* *y*) *x*) #\.) 
			(setf (elt (elt *fresh-level* *y*) *x*) symbol)
			(return))
		)
	)

(defun x-corridor (point-a point-b row)
	(loop for x from point-a to point-b 
			do
			(setf (elt (elt *fresh-level* row) x) #\.)
		)
	)

(defun y-corridor (point-a point-b column)
	(loop for y from point-a to point-b 
			do
			(setf (elt (elt *fresh-level* y) column) #\.)
		)
	)

(defun 2d-vector-to-string (map)
 	(defparameter *level-string* (make-array 0
 		:element-type 'character
 		:fill-pointer 0
 		:adjustable t))

 	(loop for row across map do
 		(loop for symbol across row do
 			(vector-push-extend symbol *level-string*)
 			)
 		(vector-push-extend #\newline *level-string*)
 		)
 	(vector-push-extend #\x *level-string*)
 		*level-string*
	)

(defun add-walls ()
	(loop for row across *fresh-level* do
		(setf (elt row 0) #\#)
		(setf (elt row (- (length row) 2)) #\#)
		)
	(loop for i from 0 to (- (length (elt *fresh-level* 0))2) do
		(setf (elt (elt *fresh-level* 0) i) #\#)
		(setf (elt (elt *fresh-level* (- (length *fresh-level*) 1)) i) #\#)
		)
	(loop for y from 0 to (- (length *fresh-level*) 1) do
		(loop for x from 0 to (- (length (elt *fresh-level* y)) 2) do
			(if (char= (elt (elt *fresh-level* y) x) #\.) (wall-off y x))
			)
		)
	)

(defun wall-off (y-pos x-pos)
	(defparameter *adjacent-cells* (list))
	(loop for y from (- y-pos 1) to (+ y-pos 1) 
		do
		(loop for x from (- x-pos 1) to (+ x-pos 1) 
			do
			(if (not(and (= y y-pos) (= x x-pos)))
				(if (char= (elt (elt *fresh-level* y) x) #\Space) (setf (elt (elt *fresh-level* y) x) #\#))
				) 
		)
	)
)