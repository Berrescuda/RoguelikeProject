;These will need to be moved later
(defparameter *cursor-x* nil)
(defparameter *cursor-y* nil)
(defparameter *y* 0)
(defparameter *x* 0)

;This is our global map. (It's a vector of vectors!)
(defparameter *map* (make-array 50 :fill-pointer 0))

;This is our global monster list.
(defparameter *monsters* (list))

(defparameter *log* (list "hello" "welcome to" "LISPCRAWL"))

;This won't exist once we're done testing stuff
(defparameter *map-string*  
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
(defun string-to-vector-map (input-string)
	;y and x are going to be used for working our way through the input string and
	;keeping track of the appropriate position in the map we're populating
	(setf *x* 0)
	(setf *y* 0)

	;currently the end of our map is denoted by an x, which should change later
	;while the current character we're on isn't an x:
	(loop while(CHAR/= (elt input-string *x*) #\x)
		;create a new row vector 
		do(defparameter *row* (make-array 50 :fill-pointer 0))
		
		;traverse the current row, pushing characters from the string 
		;into our vector, until we hit a newline
		(loop while(CHAR/= (elt input-string *x*) #\Newline)
			;push the current character into our row vector
			do (vector-push (make-instance 'tile :symbol (elt input-string *x*) :y-pos *y* :x-pos (if(> *y* 0)(- *x* (* (+ (length (elt *map* 0)) 1) *y*)) *x*)) *row*)
				;increment x
				(setf *x* (+ *x* 1)) 
				;if the current character is an x, break out of this loop
				(if(Char= (elt input-string *x*) #\x) (return)))
		
		;increment x, we're crossing over the newline
		(setf *x* (+ *x* 1))
		;when we hit a newline, push the row vector we've been building into our map 
		(vector-push *row* *map*)
		;increment y
		;(is there seriously not a better way to do this?)
		(setf *y* (+ *y* 1))))


(defun display-screen(y-start x-start)
	(setf *cursor-y* y-start)
	(setf *cursor-x* x-start)
	(setf *y* (- (slot-value *player* 'y-pos) 8))
	(setf *x* (- (slot-value *player* 'x-pos) 8))

	(loop while(< *cursor-y* (+ 17 y-start)) 
		do(loop while(< *cursor-x* (+ 17 x-start)) 
			do (if(and (>= *y* 0) (>= *x* 0) (< *y* (length *map*)) (< *x* (length (elt *map* *y*))))
				(mvaddch *cursor-y* (* *cursor-x* 2) (slot-value (elt (elt *map* *y*) *x*) 'symbol)))
			(setf *cursor-x* (+ *cursor-x* 1))
			(setf *x* (+ *x* 1)))

		(setf *cursor-y* (+ *cursor-y* 1))
		(setf *y* (+ *y* 1))

		(setf *cursor-x* x-start)
		(setf *x* (- (slot-value *player* 'x-pos) 8)))
	
	(loop for monster in *monsters*
		do(if(and 
			(>= (slot-value monster 'y-pos) (- (slot-value *player* 'y-pos) 8))
			(<= (slot-value monster 'y-pos) (+ (slot-value *player* 'y-pos) 8))
			(>= (slot-value monster 'x-pos) (- (slot-value *player* 'x-pos) 8))
			(<= (slot-value monster 'x-pos) (+ (slot-value *player* 'x-pos) 8)))
			(print-creature
				monster 
				(+(- (slot-value monster 'y-pos) (slot-value *player* 'y-pos)) (+ 8 y-start)) 
				(*(+(- (slot-value monster 'x-pos) (slot-value *player* 'x-pos)) (+ 8 x-start)) 2))))

	(print-creature *player* (+ 8 y-start) (+ 16 (* 2 x-start))))





(defun draw-frame (y-start x-start height width)
	(vline (+ y-start 1) x-start #\| (- height 1))
	(vline (+ y-start 1) (+ width x-start) #\| (- height 1))
	(hline (+ height y-start) x-start #\- width)
	(hline y-start x-start #\- width ))

;this is the function that starts the game, and calls the loop 
;that constitutes the majority of the action
(defun basic-main ()
	;plunk the player down somewhere
	(string-to-vector-map *map-string*)
	
	(init-player 10 10)
	(init-goblin 11 10)
	;load the map into the string parser
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
	(mvaddstr 20 1 (elt (reverse *log*) 2))
	(mvaddstr 21 1 (elt (reverse *log*) 1))
	(mvaddstr 22 1 (elt (reverse *log*) 0))
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
	(mvaddstr 4 39 (format nil "XP:~a" (slot-value *player* 'xp)))
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
    	(move-in-direction *player* directions)
    	;update the display
    	(display)))

;This is just a helper function since this was 
;too gross to put into the code up there unshortened
;Parameters: 	a tuple of directions to see if we can move there
;Returns: 		true if it's not blocked, false if it is
(defun not-blocked (creature directions)
	;This line of code gets the tile the character is trying to move on to
	(defparameter *target-tile* (elt (elt *map* (+ (slot-value creature 'y-pos) (elt directions 0))) (+ (slot-value creature 'x-pos) (elt directions 1))))
	
	;This line of code checks to see if the square pointed to by directions is a wall
	(CHAR/= (slot-value *target-tile* 'symbol) #\#))

(defun move-creature (creature directions)
	(defparameter *target-tile* (elt (elt *map* (+ (slot-value creature 'y-pos) (elt directions 0))) (+ (slot-value creature 'x-pos) (elt directions 1))))
	(setf (slot-value (slot-value creature 'tile) 'character) nil)
	(setf (slot-value creature 'y-pos) (+ (slot-value creature 'y-pos) (elt directions 0)))
    (setf (slot-value creature 'x-pos) (+ (slot-value creature 'x-pos) (elt directions 1)))
	(setf (slot-value creature 'tile) *target-tile*)
	(setf (slot-value *target-tile* 'character) creature)
	)

(defun move-in-direction (creature directions)
	(if(not-blocked creature directions)
    		;if it's not blocked, we move our player to the square
    		(if (not(slot-value *target-tile* 'character)) (move-creature creature directions) (attack creature (slot-value *target-tile* 'character)))))

(defun attack (attacker defender)
	(setf (slot-value defender 'currenthp) (- (slot-value defender 'currenthp) (slot-value attacker 'power)))
	(when(<= (slot-value defender 'currenthp) 0)(die defender)
		(setf (slot-value attacker 'xp) (+ (slot-value attacker 'xp) 1)))
	)

(defun die (departed)
	(setf *log* (append *log* (list (format nil "~a has died." (slot-value departed 'name)))))
	(setf *monsters* (remove departed *monsters*))
	(setf (slot-value (slot-value departed 'tile) 'character) nil)

	)
;Class Definitions

;base creature class
(defclass creature () 
	(maxhp
	currenthp
	x-pos
	y-pos
	name
	symbol
	(power
		:initform 1)
	(xp
		:initform 0)
	tile
	))

(defclass tile ()
	((y-pos
		:initarg :y-pos) 
	(x-pos
		:initarg :x-pos)
	(symbol
		:initarg :symbol)
	(character
		:initform nil)
	(path-value
		:initform 0)))

;This is perhaps the least elegant way to do this imaginable
(defun init-player (y x)
	(defparameter *player* (make-instance 'creature))
	(setf (slot-value *player* 'name) "foobar")
	(setf (slot-value *player* 'maxhp) 10)
	(setf (slot-value *player* 'currenthp) 10)
	(setf (slot-value *player* 'y-pos) y)
	(setf (slot-value *player* 'x-pos) x)
	(setf (slot-value *player* 'symbol) #\@)
	(setf (slot-value *player* 'tile) (elt (elt *map* y) x))
	(setf (slot-value (elt (elt *map* y) x)'character) *player*))


(defun init-goblin (y x)
	(defparameter *goblin* (make-instance 'creature))
	(setf (slot-value *goblin* 'name) "Space goblin")
	(setf (slot-value *goblin* 'maxhp) 5)
	(setf (slot-value *goblin* 'currenthp) 5)
	(setf (slot-value *goblin* 'y-pos) y)
	(setf (slot-value *goblin* 'x-pos) x)
	(setf (slot-value *goblin* 'symbol) #\g)
	(setf (slot-value *goblin* 'tile) (elt (elt *map* y) x))
	(setf (slot-value (elt (elt *map* y) x)'character) *goblin*)

	;feels a little pythonic, but it does what I want it to do
	(setf *monsters* (append *monsters* (list *goblin*)))
	)

(defun print-creature (creature y x)
	(attrset :cred)
	(mvaddch y x (slot-value creature 'symbol)) 
	(attrset :cgray))

;curses helper functions
(defun hline (y-start x-start symbol length)
	(loop for x from x-start to (+ x-start length) do (mvaddch y-start x symbol))
	)

(defun vline (y-start x-start symbol length)
	(loop for y from y-start to (+ y-start length) do (mvaddch y x-start symbol))
	)

(defun list-adjacent-tiles (center-tile)
	(defparameter adjacent-tiles (list))
	(loop for y from (- (slot-value center-tile 'y-pos) 1) to (+ (slot-value center-tile 'y-pos) 1)
		do(loop for x from (- (slot-value center-tile 'x-pos) 1) to (+ (slot-value center-tile 'x-pos) 1)
			do(if (not(and (= y (slot-value center-tile 'y-pos)) (= x (slot-value center-tile 'x-pos)))) (setf adjacent-tiles
							(append adjacent-tiles (list (elt (elt *map* y) x)))		
						))	
		)
	)
	adjacent-tiles
)

(defun find-path (creature target)

	)
