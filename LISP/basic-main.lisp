;These will need to be moved later
(defparameter *cursorX* nil)
(defparameter *cursorY* nil)
(defparameter *y* 0)
(defparameter *x* 0)

;This is our global map. (It's a vector of vectors!)
(defparameter *map* (make-array 50 :fill-pointer 0))

;This won't exist once we're done testing stuff
(defparameter *mapString*  
"   ####                 ###               
   #..############      #.#               
   #.##..........#      #.#               
   #.##.####.###.########.################
   #.##.#  #.###.........................#
   #.##.#  #.....########.################
   #..>.####%#####      #.#               
   #######...#          #.#               
         #.###          #.#               
   ###   #.#            #.#               
   #.##  #.#            #.#               
   #..####.##############.#               
   #.##...........<.......#               
   #.##.####.###.########.#               
   #.##.#  #.###.#      #.#               
   #.##.#  #.....#      #.#               
   #....####.#####      #.#               
   #######...#          #.#               
         #.###          #.#               
         ###            ###               
x")

;string-to-vector-map takes a string and populates our vector of vectors with it
;Parameters: 	string (a string representing our map, as above)
;Returns: 		nothing (but updates our global map with correct info)
(defun string-to-vector-map (inputString)
	;y and x are going to be used for working our way through the input string and
	;keeping track of the appropriate position in the map we're populating
	(setf *x* 0)
	(setf *y* 0)

	;currently the end of our map is denoted by an x, which should change later
	;while the current character we're on isn't an x:
	(loop while(CHAR/= (elt inputString *x*) #\x)
		;create a new row vector 
		do(defparameter *row* (make-array 50 :fill-pointer 0))
		
		;traverse the current row, pushing characters from the string 
		;into our vector, until we hit a newline
		(loop while(CHAR/= (elt inputString *x*) #\Newline)
			;push the current character into our row vector
			do (vector-push (elt inputString *x*) *row*)
				;increment x
				(setf *x* (+ *x* 1)) 
				;if the current character is an x, break out of this loop
				(if(Char= (elt inputString *x*) #\x) (return)))
		
		;increment x, we're crossing over the newline
		(setf *x* (+ *x* 1))
		;when we hit a newline, push the row vector we've been building into our map 
		(vector-push *row* *map*)
		;increment y
		;(is there seriously not a better way to do this?)
		(setf *y* (+ *y* 1))))


(defun display-screen(y-start x-start)
	(setf *cursorY* y-start)
	(setf *cursorX* x-start)
	(setf *y* (- (slot-value *player* 'yPos) 8))
	(setf *x* (- (slot-value *player* 'xPos) 8))

	(loop while(< *cursorY* (+ 17 y-start)) 
		do(loop while(< *cursorX* (+ 17 x-start)) 
			do (if(and (>= *y* 0) (>= *x* 0) (< *y* (length *map*)) (< *x* (length (elt *map* *y*))))
				(mvaddch *cursorY* (* *cursorX* 2) (elt (elt *map* *y*) *x*)))
			(setf *cursorX* (+ *cursorX* 1))
			(setf *x* (+ *x* 1)))

		(setf *cursorY* (+ *cursorY* 1))
		(setf *y* (+ *y* 1))

		(setf *cursorX* x-start)
		(setf *x* (- (slot-value *player* 'xPos) 8)))
	(attrset :cred)
	(mvaddstr (+ 8 y-start) (+ 16 (* 2 x-start)) "@") (attrset :cgray))

(defun draw-frame (y-start x-start height width)
	(vline (+ y-start 1) x-start #\| (- height 1))
	(vline (+ y-start 1) (+ width x-start) #\| (- height 1))
	(hline (+ height y-start) x-start #\- width)
	(hline y-start x-start #\- width ))

;this is the function that starts the game, and calls the loop 
;that constitutes the majority of the action
(defun basic-main ()
	;plunk the player down somewhere
	(init-player 10 10)
	;load the map into the string parser
	(string-to-vector-map *mapString*)
	;start curses
	(connect-console)
	;print out the screen
	(display)
	;begin main loop
	(main-loop)
	;end curses
	(endwin))

;display handles everything that happens on the screen
(defun display()
	;clear the string
	(erase)
	;draw a box around the map screen
	(draw-frame 0 0 18 36)
	;fill the map box with what the player can see
	(display-screen 1 1)
	;draw a box around the log zone
	(draw-frame 19 0 4 36)
	;draw a box around the stat window
	(draw-frame 0 38 18 23)
	
	;prinr various details about the player
	;name
	(mvaddstr 1 39 "Name:")
	(mvaddstr 1 44 (slot-value *player* 'name))
	;hp
	(mvaddstr 2 39 "hp:")
	(attrset :cgreen)
	(hline 2 42 #\= 9)
	(attrset :cgray)
	(mvaddstr 2 53 "10/10")
	;xp
	(mvaddstr 4 39 "XP:0")
	;level of the dungeon
	(mvaddstr 5 39 "Current Level: 0")
	;player's inventory
	(mvaddstr 6 39 "Inventory:")
	)

;This is our main in-game loop, it currently handles player commands and displays the map
(defun main-loop ()
	(loop
		;initialize a small vector for the directions we're going to move in this turn
		(defvar directions (vector 0 0))
		;get a character from the keyboard
  		(case (curses-code-char (getch))
  			;q means quit the game
  			((#\q) (return))
  			;different directions for different keystrokes
  			;straight in any direction
  			((#\s #\2) (setf directions (vector 1 0)))
  			((#\w #\8) (setf directions (vector -1 0)))
  			((#\d #\6) (setf directions (vector 0 1)))
  			((#\a #\4) (setf directions (vector 0 -1)))
  			;the diagonals
  			((#\1) (setf directions (vector 1 -1)))
 			((#\3) (setf directions (vector 1 1)))
  			((#\7) (setf directions (vector -1 -1)))
  			((#\9) (setf directions (vector -1 1))))
  		;check to see if the square we're trying to move to is blocked
    	(if(not-blocked directions)
    		;if it's not blocked, we move our player to the square
    		(progn
    			(setf (slot-value *player* 'yPos) (+ (slot-value *player* 'yPos) (elt directions 0)))
    			(setf (slot-value *player* 'xPos) (+ (slot-value *player* 'xPos) (elt directions 1)))))
    	;update the display
    	(display)))

;This is just a helper function since this was 
;too gross to put into the code up there unshortened
;Parameters: 	a tuple of directions to see if we can move there
;Returns: 		true if it's not blocked, false if it is
(defun not-blocked (directions)
	;This line of code checks to see if the square pointed to by directions is a wall
	(CHAR/= (elt (elt *map* (+ (slot-value *player* 'yPos) (elt directions 0))) (+ (slot-value *player* 'xPos) (elt directions 1))) #\#))

;Class Definitions

;base creature class
(defclass creature () 
	(maxhp
	currenthp
	xPos
	yPos
	name
	symbol
	))

;This is perhaps the least elegant way to do this imaginable
(defun init-player (y x)
	(defparameter *player* (make-instance 'creature))
	(setf (slot-value *player* 'name) "foobar")
	(setf (slot-value *player* 'maxhp) 10)
	(setf (slot-value *player* 'currenthp) 10)
	(setf (slot-value *player* 'yPos) y)
	(setf (slot-value *player* 'xPos) x)
	(setf (slot-value *player* 'symbol) "@")
	)

;curses helper functions
(defun hline (y-start x-start symbol length)
	(loop for x from x-start to (+ x-start length) do (mvaddch y-start x symbol))
	)

(defun vline (y-start x-start symbol length)
	(loop for y from y-start to (+ y-start length) do (mvaddch y x-start symbol))
	)