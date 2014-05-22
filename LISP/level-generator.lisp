
;our generate-level function creates a string representing a new level
;Parameters: 	height of the level, width, and a variable determining 
;				whether or not this is the first level in the dungeon
;Returns: 		a string that can be easily parsed into a level map object
(defun generate-level (height width first)
	;the height and width variables should be one less
	;than the values specified when calling the function,
	;because both lists will start at 0 instead of 1
	(defparameter *height* (- height 1))
	(defparameter *width* (- width 1))

	;Set up an empty map
	(initialize-level)
	;Initialize a list that will hold the rooms on the level,
	;give it one randomly generated room
	(defparameter *rooms* (list (init-room)))
	;create 7 to 14 rooms on the level
	(loop for i from 1 to (+ 7 (random 7))
		;create a new room, 
		;then build a hallway from that room to an existing room chosen at random
		do (connect (elt *rooms* (random (length *rooms*))) (init-room)))

	;add walls to the map so there are no inexplicable holes in the dungeon
	(add-walls)

	;Put an up and down staircase somewhere in the level
	(place-in-room (elt *rooms* (random (length *rooms*))) #\<)
	(place-in-room (elt *rooms* (random (length *rooms*))) #\>)

	;for each room there's a 1/4 chance of it having a goblin in it,
	;and a 1/7 chance of having a potion
	(loop for room in *rooms* do
		(if (= (random 4) 0) 
			(place-in-room room #\g))
		(if (= (random 7) 0) 
			(place-in-room room #\%)))

	;if this is the first level in the dungeon, put the player in it somewhere
	(if first 
		(place-in-room (elt *rooms* (random (length *rooms*))) #\@))

	;convert our 2d vector into a string
	(2d-vector-to-string *fresh-level*))

;initialize-level creates a vector of character vectors
;Parameters: 	none
;Returns: 		Nothing of interest
(defun initialize-level ()
	;Create a new vector to hold our rows of the map 
	;(give it some padding size-wize so we don't break anything)
	(defparameter *fresh-level* (make-array (+ *height* 5) :fill-pointer 0))
	;for each element in the first vector
	(loop for i from 0 to *height*
		;make a new row and set the first vector's element to it
		do (vector-push (make-array (+ *width* 5) :fill-pointer 0) *fresh-level*)
		;then for each element in the row, set that element to " "
		(loop for j from 0 to *width* 
			do (vector-push #\Space (elt *fresh-level* i)))))

;define what constitues a "room"
(defclass chamber ()
	;a room has a height,
	((height
		:initarg :height)
	;a width
	(width
		:initarg :width)
	;coordinates for it's upper left hand corner
	(y-pos
		:initarg :y-pos)
	(x-pos
		:initarg :x-pos) 
	;and a boolean keeping track of whether or not 
	;it's connected to the rest of the level
	(connected :initform nil)))

;our init-room function randomly generates a new room
;Parameters: 	none
;Returns: 		a new, randomly generated room
(defun init-room ()
	;create the new room
	(defparameter *new-room* (make-instance 'chamber))
	;pick random values for height, width, x-pos and y-pos
	(setf (slot-value *new-room* 'height) (+ (random 4) 2))
	(setf (slot-value *new-room* 'width) (+ (random 4) 2))
	(setf (slot-value *new-room* 'y-pos) (random (- *height* (slot-value *new-room* 'height))))
	(setf (slot-value *new-room* 'x-pos) (random (- *width* (slot-value *new-room* 'width))))
	;print a '.' to every space that falls in the room
	(loop for y from (slot-value *new-room* 'y-pos) to (+ (slot-value *new-room* 'y-pos) (- (slot-value *new-room* 'height) 1))
		do (loop for x from (slot-value *new-room* 'x-pos) to (+ (slot-value *new-room* 'x-pos) (- (slot-value *new-room* 'width) 1) )
			do (setf (elt (elt *fresh-level* y) x) #\.)))
	;return our new room
	*new-room*)

;our connect function connects a room to the rest of the level
;Parameters: 	a room already connected to the level, a room that isn't connected
;Returns: 		nothing of interest
(defun connect (connected-room new-room)
	;pick a random point within the new room and the old room, and pick out the x and y coordinates of that point
	(defparameter *y-new* (+ (slot-value *new-room* 'y-pos) (random (slot-value new-room 'height))))
	(defparameter *x-new* (+ (slot-value *new-room* 'x-pos) (random (slot-value new-room 'width))))
	(defparameter *y-old* (+ (slot-value connected-room 'y-pos) (random (slot-value new-room 'height))))
	(defparameter *x-old* (+ (slot-value connected-room 'x-pos) (random (slot-value new-room 'width))))
	
;first we create a horizontal corridor towards the room we're connecting to

	;If the x-position we're traveling to is greater than 
	;the x-position we're starting from:
	(if (> *x-old* *x-new*)
		;draw a corridor from our starting point to the coordinates (y-new, x-old)
		(x-corridor *x-new* *x-old* *y-new*)
		;otherwise draw a corridor from (y-new, x-old) to our starting point
		(x-corridor *x-old* *x-new* *y-new*))
	
;then we walk from the end point of that horizontal corridor to the point we chose in the connected room
	
	;If the y-position we're traveling to is greater than 
	;the y-position we're starting from:
	(if (> *y-old* *y-new*)
		;draw a corridor from where our horizontal corridor 
		;lines up with the old room's x coordinate to the point we chose in the old room
		(y-corridor *y-new* *y-old* *x-old*)
		;otherwise start at the point in the old room we chose and draw it down to the end
		;of our horizontal corridor
		(y-corridor *y-old* *y-new* *x-old*))
	;add our room to the list of connected rooms
	(setf *rooms* (append *rooms* (list *new-room*))))

;our place-in-room function puts a symbol randomly on a place in a room
;parameters: 	the room we're putting the symbol in, the symbol
;returns: 		nothing of interest
(defun place-in-room (room symbol)
	;keep track of whether or not we have successfully found an empty space
	(defvar success nil)
	(loop
		;pick a random point in the room
		do (defparameter *x* (+ (slot-value room 'x-pos) (random (slot-value room 'width))))
		(defparameter *y* (+ (slot-value room 'y-pos) (random (slot-value room 'height))))
		;if that point is empty, break out of our loop, otherwise we try again
		(when (char= (elt (elt *fresh-level* *y*) *x*) #\.)
			;draw our symbol on the point we picked
			(setf (elt (elt *fresh-level* *y*) *x*) symbol)
			;break out of the loop
			(return))))

;x-corridor draws a horizontal line of empty spaces from point a to point b
;parameters: 	coordinates of point a and point b
;returns: 		nothing of interest
(defun x-corridor (point-a point-b row)
	(loop for x from point-a to point-b
			;for each space in between point a and point b,
			;stamp down a period 
			do (setf (elt (elt *fresh-level* row) x) #\.)))

;y-corridor draws a vertical line of empty spaces from point a to point b
;parameters: 	coordinates of point a and point b
;returns: 		nothing of interest
(defun y-corridor (point-a point-b column)
	(loop for y from point-a to point-b 
			;for each space in between point a and point b,
			;stamp down a period 
			do (setf (elt (elt *fresh-level* y) column) #\.)))

;2d-vector-to-string takes a 2-d vector of characters and formats it
;into a string that can be read by the string parser in basic-main.lisp
;parameters: 	a two dimensional vector of characters
;returns: 		a string representing that vector
(defun 2d-vector-to-string (map)
	;set up an empty string that we can add characters to
 	(defparameter *level-string* (make-array 0
 		:element-type 'character
 		:fill-pointer 0
 		:adjustable t))

 	;for every row in our vector
 	(loop for row across map
 		;for every character in that row
 		do (loop for symbol across row 
 			;push that character onto our string
 			do (vector-push-extend symbol *level-string*))
 		;at the end of every row, put a newline
 		(vector-push-extend #\newline *level-string*))

 	;put an x at the end of the string indicating the end of the string
 	(vector-push-extend #\x *level-string*)
 		*level-string*)

;add-walls goes through the map and puts walls where they need to be
;parameters: 	none
;returns: 		nothing of interest
(defun add-walls ()

	;draw a border of walls on the left and right of the level
	(loop for row across *fresh-level* 
		;set the left and right sides of the map to walls
		do (setf (elt row 0) #\#)
		(setf (elt row (- (length row) 2)) #\#))

	;draw the top and bottom rows of walls
	(loop for i from 0 to (- (length (elt *fresh-level* 0))2)
		;set each character the top row and bottom row to '#'
		do (setf (elt (elt *fresh-level* 0) i) #\#)
		(setf (elt (elt *fresh-level* (- (length *fresh-level*) 1)) i) #\#))

	;for every square on the map
	(loop for y from 0 to (- (length *fresh-level*) 1)
		do (loop for x from 0 to (- (length (elt *fresh-level* y)) 2)
			;if that square is empty floor, wall off it's adjacent spaces
			do (if (char= (elt (elt *fresh-level* y) x) #\.) 
				(wall-off y x)))))

;wall-off checks every square adjacent to a particular square 
;and sets it to a wall if it should be one
;parameters: 	the coordinates of the square to wall off
;returns: 		nothing of interest
(defun wall-off (y-pos x-pos)
	;for each square that's adjacent to the one we're given
	(loop for y from (- y-pos 1) to (+ y-pos 1) 
		do (loop for x from (- x-pos 1) to (+ x-pos 1)
			do (if (not(and (= y y-pos) (= x x-pos)))
				;if that character is nothing, set it to a wall, 
				;since it's adjacent to a square of floor
				(if (char= (elt (elt *fresh-level* y) x) #\Space) 
					(setf (elt (elt *fresh-level* y) x) #\#))))))