;These will need to be moved later
(load "level-generator.lisp")
(defparameter *cursor-x* nil)
(defparameter *cursor-y* nil)
(defparameter *y* 0)
(defparameter *x* 0)
(defparameter *object-stack* (list))
(defparameter *down-stairs* (list 0 0))
(defparameter *up-stairs* (list 0 0))

(defparameter *dungeon* (list))

;This is our global map. (It's a vector of vectors!)
(defparameter *map* (make-array 50 :fill-pointer 0))

;This is our global monster list.
(defparameter *monsters* (list))

(defparameter *log* (list "hello" "welcome to" "LISPCRAWL"))

;string-to-vector-map takes a string and populates our vector of vectors with it
;Parameters: 	string (a string representing our map, as above)
;Returns: 		nothing (but updates our global map with correct info)
(defun string-to-vector-map (input-string)
	(defparameter *map* (make-array 50 :fill-pointer 0))

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
			do
			(case (elt input-string *x*)

				((#\g #\@ #\%)
					(setf *object-stack* (append *object-stack* (list (list *y* (grid-x *x*) (elt input-string *x*)))))
					(setf (elt input-string *x*) #\.)
				)
				((#\>) (setf *down-stairs* (list *y* (grid-x *x*))))
				((#\<) (setf *up-stairs* (list *y* (grid-x *x*))))
			)

			;push the current character into our row vector
			(vector-push (make-instance 'tile :symbol (elt input-string *x*) :y-pos *y* :x-pos (grid-x *x*)) *row*)
			;increment x
			(setf *x* (+ *x* 1)) 
			;if the current character is an x, break out of this loop
			(if(Char= (elt input-string *x*) #\x) (return)))
		
		;increment x, we're crossing over the newline
		(setf *x* (+ *x* 1))
		;when we hit a newline, push the row vector we've been building into our map 
		(vector-push *row* *map*)
		;increment y
		(setf *y* (+ *y* 1)))
	*map*
	)

;grid-x is just a wrapper for the arithmetic that needs to be done to 
;get the cursor's position in our string to be converted to an appropriate x coordinate
(defun grid-x (x)

	(if(> *y* 0) ;If we're past the first row
		;Return the cursor's value minus the width of the map 
		;times the number of rows we've built
		(- x (* (+ (length (elt *map* 0)) 1) *y*)) 
		;Otherwise return x
		x)
	)

;This function takes all of the characters and items that have been pushed on to our object stack,
;and populates the current map with them.
(defun unpack-object-stack ()
	;We keep popping objects off our stack (not actually a stack, but I'm pretending it's one)
	;until it's empty
	(loop while (> (length *object-stack*) 0)
		;We have a case for each object we could be given
		;So we check the third element of the list we get from the stack 
		;(the first two elements are it's x and y coordinates)
		do (case(elt (elt *object-stack* 0) 2)
				;g means goblin, so we put a goblin at map coordinates x and y, 
				;which we grab from the list at the front of the stack (I need to stop calling it that)
				((#\g) (init-goblin (elt (elt *object-stack* 0) 0) (elt (elt *object-stack* 0) 1) ))
				;an @ symbol means the player, in much the same way as the goblin,
				;we plunk the player down wherever the coordinates say
				((#\@) (init-player (elt (elt *object-stack* 0) 0) (elt (elt *object-stack* 0) 1) ))
				;The % symbol means we have a potion, and that's handled in a slightly different way
				;we still get the x and y coordinates, but instead of calling an init function, 
				;we modify the specific tile's item list, and put a potion there
				((#\%) (setf (slot-value (elt (elt *map* (elt (elt *object-stack* 0) 0)) (elt (elt *object-stack* 0) 1)) 'items) 
				(list (make-instance 'item :symbol #\% :name "potion")))))
		;Once we've placed our object, it's time to take it off of our objects list
		;so we just clip the first element off of it and start our loop over if there are any elements left
		(setf *object-stack* (remove (elt *object-stack* 0) *object-stack*))))

;display-screen draws the main map of the game, complete with character in the center,
;drawing the block of spaces around the player that fall within their field of vision
;parameters: 	The y and x coordinates that on the terminal where the upper-left corner of the map will be drawn
(defun display-screen(y-start x-start)
	;the cursors will keep track of which position on the actual terminal we're currently on
	(setf *cursor-y* y-start)
	(setf *cursor-x* x-start)
	;While *x* and *y* keep track of our coordinates inside the game level
	(setf *y* (- (slot-value *player* 'y-pos) 8))
	(setf *x* (- (slot-value *player* 'x-pos) 8))

	;iterate over every row that will be displayed on the screen
	(loop while(< *cursor-y* (+ 17 y-start)) 
		;iterate over that row, printing the character at *y*, *x*
		do(loop while(< *cursor-x* (+ 17 x-start)) 
			;If we're going to print a character, a series of conditions have to be met.
				;first, y and x must not be negative, we shouldn't try to print tiles that don't exist
			do (when(and (>= *y* 0) (>= *x* 0) 
				;second, x and y can't be *greater* than the width or height of the map,
				;those tiles don't exist, we can't draw them
				(< *y* (length *map*)) (< *x* (length (elt *map* *y*))) 
				;third, the tile must have been seen by the player at least once
				(slot-value (elt (elt *map* *y*) *x*) 'explored))
				
				;once all those conditionals are met, 
				;if the tile isn't visible, draw it in purple 
				;(because it has been explored, we just can't see it right now)
				(if (not(slot-value (elt (elt *map* *y*) *x*) 'visible)) (attrset :cpurple))

				;Actually draw the character at map[*y*][*x*] at position [*cursor-y*][*cursor-x*] on the screen.
				(mvaddch *cursor-y* (* *cursor-x* 2) (slot-value (elt (elt *map* *y*) *x*) 'symbol))
				;If the tile has items on it, print the symbol of the first item the tile has
				(when (slot-value (elt (elt *map* *y*) *x*) 'items) 
					;if the item is visible, it's blue, otherwise, still purple
					(if (slot-value (elt (elt *map* *y*) *x*) 'visible) (attrset :cblue))
					(mvaddch *cursor-y* (* *cursor-x* 2) (slot-value (elt (slot-value (elt (elt *map* *y*) *x*) 'items) 0) 'symbol))
					)
				;change the color of the characters we print back to gray
				(attrset :cgray))
			;Increment both x's
			(setf *cursor-x* (+ *cursor-x* 1))
			(setf *x* (+ *x* 1)))
		
		;After a row is done, increment both y's
		(setf *cursor-y* (+ *cursor-y* 1))
		(setf *y* (+ *y* 1))

		;Then set both of the x values back to their initial states 
		;(start printing on the left side of the screen again)
		(setf *cursor-x* x-start)
		(setf *x* (- (slot-value *player* 'x-pos) 8)))
	
	;for every monster on the level, print that monster where it is if we can see it
	(loop for monster in *monsters*
		do(if(and 
			;if the monster's coordinates are within the bounds of what we can see
			(>= (slot-value monster 'y-pos) (- (slot-value *player* 'y-pos) 8))
			(<= (slot-value monster 'y-pos) (+ (slot-value *player* 'y-pos) 8))
			(>= (slot-value monster 'x-pos) (- (slot-value *player* 'x-pos) 8))
			(<= (slot-value monster 'x-pos) (+ (slot-value *player* 'x-pos) 8))
			;and we can see it
			(slot-value (slot-value monster 'tile) 'visible))
			;print the monster
			(print-creature
				monster 
				;at on the square it occupies
				(+(- (slot-value monster 'y-pos) (slot-value *player* 'y-pos)) (+ 8 y-start))
				(*(+(- (slot-value monster 'x-pos) (slot-value *player* 'x-pos)) (+ 8 x-start)) 2))))
	;print the player in the center of our screen
	(print-creature *player* (+ 8 y-start) (+ 16 (* 2 x-start))))

;draw-frame takes a y cord, an x cord, a height and a width, and then
;draws a box according to those specifications
(defun draw-frame (y-start x-start height width)
	;draw the leftmost line
	(vline (+ y-start 1) x-start #\| (- height 1))
	;draw the rightmost line
	(vline (+ y-start 1) (+ width x-start) #\| (- height 1))
	;draw the bottom line
	(hline (+ height y-start) x-start #\- width)
	;draw the top line
	(hline y-start x-start #\- width ))

;this is the function that starts the game, and calls the loop 
;that constitutes the majority of the action
(defun basic-main ()
	;Generate a small dungeon
	(loop for i from 0 to 4
		do(init-level))

	;set the *map* parameter to the map of the first level
	(setf *map* (slot-value (elt *dungeon* 0) 'level-map))
	;set the *monsters* parameter to the monsters that exist on the first level
	(setf *monsters* (slot-value (elt *dungeon* 0) 'monsters))

	(get-line-of-sight *player*)
	
	;start curses
	(connect-console)
	;print out the screen
	(display)
	;begin main loop
	(main-loop)
	;end curses once the player is done
	(endwin))

;display handles all of the visual stuff that happens on the screen
(defun display()
	;clear the string
	(erase)
	;draw a box around the map screen
	(draw-frame 0 0 18 36)
	;fill the map box with what the player can see
	(display-screen 1 1)
	;draw a box around the log zone
	(draw-frame 19 0 4 36)
	;draw the last three entries in the log
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

	;This prints the player's health bar,
	;The way I'm calculating the length of 
	;the bar here isn't going to cut it when 
	;the player's max-hp can actually increase, but for now it's fine
	(hline 2 42 #\= (slot-value *player* 'currenthp))
	(attrset :cgray)
	;print the player's max and current hp
	(mvaddstr 2 53 (format nil "~a/~a" (slot-value *player* 'currenthp) (slot-value *player* 'maxhp)))
	;xp
	(mvaddstr 4 39 (format nil "XP:~a" (slot-value *player* 'xp)))
	;level of the dungeon the player is on
	(mvaddstr 5 39 (format nil "Current Level:~a" (slot-value *player* 'current-level)))
	;player's inventory
	(mvaddstr 6 39 "Inventory:")
	;print the name of every item in the inventory
	(loop for i from 0 to (- (length (slot-value *player* 'inventory)) 1)
		do
		(mvaddstr (+ 7 i) 39 (slot-value (elt (slot-value *player* 'inventory) i) 'name))
		)
	)

;This is our main in-game loop, the player takes a turn, the monsters take a turn,
;and the screen is updated.
(defun main-loop ()
	;when the player is done, the game is over
	(defparameter *done* nil)
	(loop while (not *done*)
		do
		;initialize a small vector for the directions we're going to move in this turn
		(player-turn)
    	(monsters-turn)
    	;update the display
    	(display)))

;This is the function that handles the player's turn.
;It waits for input and then takes actions for the player
;based on what character is entered
(defun player-turn ()
	;initialize a small list of directions
	(defvar directions (list 0 0))
		;get a character from the keyboard
  		(case (curses-code-char (getch))
  			;q means quit the game
  			((#\q) (setf *done* t))
  			;different directions for different keystrokes
  			;straight in any direction
  			((#\s #\2) (setf directions (list 1 0)))	;down
  			((#\w #\8) (setf directions (list -1 0)))	;up
  			((#\d #\6) (setf directions (list 0 1)))	;right
  			((#\a #\4) (setf directions (list 0 -1)))	;left
  			
  			;the diagonals
  			((#\1) (setf directions (list 1 -1)))		;down-left
 			((#\3) (setf directions (list 1 1)))		;down-right
  			((#\7) (setf directions (list -1 -1)))		;up-left
  			((#\9) (setf directions (list -1 1)))		;up-right
  			
  			;don't move, pass the turn
  			((#\5) (setf directions (list 0 0)))

  			;special actions
  			;go up or down stairs
  			((#\>) (go-down-stairs *player*) 
  				(setf directions (list 0 0)))
  			((#\<) (go-up-stairs *player*)
  				(setf directions (list 0 0)))

  			;grab and use items
  			((#\g) (grab-item *player*)
  				(setf directions (list 0 0)))
  			((#\u) (drink-potion *player*)
  				(setf directions (list 0 0)))
  			)
  		;check to see if the square we're trying to move to is blocked,
  		;if it's not, move into that square
    	(if (not (equal (list 0 0) directions)) (move-in-direction *player* directions))
    	;refresh line of sight
   		(get-line-of-sight *player*)
   		;regenerate slowly
   		(heal-naturally *player*)
	)

;Our grab item function takes a character and grabs whatever item is on it's current tile
(defun grab-item (character)
	;if there is an item, pick it up
	(if (> (length (slot-value (slot-value character 'tile) 'items)) 0) 
		(progn
			;add the item to the character's inventory
			(setf (slot-value character 'inventory) (append (slot-value character 'inventory)
				(list (elt (slot-value (slot-value character 'tile) 'items) 0))))
			;remove the item from the tile's list of items
			(setf (slot-value (slot-value character 'tile) 'items) (remove (elt (slot-value (slot-value character 'tile) 'items)0) (slot-value (slot-value character 'tile) 'items)))
			;let the player know they have a new item
			(log-message (format nil "picked up a potion")))
		;Otherwise, send a message to the player.
		(log-message "there are no items to grab here"))
	)

;Our drink-potion function takes a character and makes them use a potion in their inventory if they can.
(defun drink-potion (character)
	;if the character has any items in their inventory
	(if (> (length (slot-value character 'inventory)) 0)
		(progn
			;Increase the character's hp by 5
			(setf (slot-value character 'currenthp) (+ 5 (slot-value character 'currenthp)))
			;but not above the maximum
			(if (< (slot-value character 'maxhp) (slot-value character 'currenthp))
				(setf (slot-value character 'currenthp) (slot-value character 'maxhp)))
			;message the player (in the futrue this should use the name of the 
			;character drinking the potion instead of "you" but seeing as how
			;only the player can drink potions right now, this will be fine)
			(log-message "you drink a potion")
			;remove the potion from the player's inventory
			(setf (slot-value *player* 'inventory) (remove (elt (slot-value *player* 'inventory) 0) (slot-value *player* 'inventory))))
		;if there are no items in the player's inventory, let the player know and exit our function
		(log-message "you don't have any items to use")
	)
)

;go-down-stairs takes a character and attempts to go down a staircase
;Parameters 	the character taking the action
;Returns 		nothing
(defun go-down-stairs (character)
	;If the character is standing on a down-staircase:
	(if (char= (slot-value (slot-value character 'tile) 'symbol) #\>)
		(progn
			;Set the tile the character is standing on to hold no character
			(setf (slot-value (slot-value character 'tile) 'character) nil)
			;get the next level we're going to from the dungeon list
			(defparameter *next-level* (elt *dungeon* (+ (slot-value character 'current-level) 1)))
			;Set the global *map* variable to the map of the next level
			(setf *map* (slot-value *next-level* 'level-map))
			;Update the list of monsters to the monsters on the next level
			;(This could stand to be refactored)
			(setf *monsters* (slot-value *next-level* 'monsters))
			;Set the character's current-level value to the level number of the new level
			(setf (slot-value character 'current-level) (+ (slot-value character 'current-level) 1))

			;Set the character's x and y position to the coordinates of the next level's up staircase
			(setf (slot-value character 'y-pos) (elt (slot-value *next-level* 'up-stairs) 0))
			(setf (slot-value character 'x-pos) (elt (slot-value *next-level* 'up-stairs) 1))

			;Set the tile field of our character to the new tile we're standing on
			(setf (slot-value character 'tile) (elt (elt *map* (slot-value character 'y-pos)) (slot-value character 'x-pos)))
			;Set the character field of our new tile to our character
			(setf (slot-value (slot-value character 'tile) 'character) character)
			)
		;if we're not on a down staircase, let the player know.
		(log-message "You can't go down here")
		)
	)

;go-up-stairs takes a character and attempts to go up a staircase
;Parameters 	the character taking the action
;Returns 		nothing
(defun go-up-stairs (character)
	;If the character is standing on an up-staircase:
	(if (char= (slot-value (slot-value character 'tile) 'symbol) #\<)
		(progn
			;Set the tile the character is standing on to hold no character
			(setf (slot-value (slot-value character 'tile) 'character) nil)
			;get the next level we're going to from the dungeon list
			(defparameter *previous-level* (elt *dungeon* (- (slot-value character 'current-level) 1)))
			;Set the global *map* variable to the map of the new level
			(setf *map* (slot-value *previous-level* 'level-map))
			;Update the list of monsters to the monsters on the new level
			;(This could stand to be refactored)
			(setf *monsters* (slot-value *previous-level* 'monsters))
			;Set the character's current-level value to the level number of the new level
			(setf (slot-value character 'current-level) (- (slot-value character 'current-level) 1))

			;Set the character's x and y position to the coordinates of the next level's up staircase
			(setf (slot-value character 'y-pos) (elt (slot-value *previous-level* 'down-stairs) 0))
			(setf (slot-value character 'x-pos) (elt (slot-value *previous-level* 'down-stairs) 1))
			;Set the tile field of our character to the new tile we're standing on
			(setf (slot-value character 'tile) (elt (elt *map* (slot-value character 'y-pos)) (slot-value character 'x-pos)))
			;Set the character field of our new tile to our character
			(setf (slot-value (slot-value character 'tile) 'character) character)
			)
		;If we're not standing on an up staircase, let the player know.
		(log-message "You can't go up here")
		)
	)

;monsters-turn loops throuh every monster on the map and gives them actions to perform if applicable
;Parameters: 	none
;returns: 		nothing important
(defun monsters-turn ()
	;For each monster on the level
	(loop for monster in *monsters* 
		do
		;update the monster's line of sight
		(get-line-of-sight monster)
		;If the monster can see the player, find a shortest path to the player
		(if (slot-value monster 'can-see-player) (find-path monster (slot-value *player* 'tile)))
		;If the monster is currently following a path,
		(when (> (length (slot-value monster 'direction-list)) 0)
		;-then move along the path
		(move-in-direction monster (elt (slot-value monster 'direction-list) 0))
		;Remove the direction we just followed from the monster's direction list
		(setf (slot-value monster 'direction-list) (remove (elt (slot-value monster 'direction-list) 0) 
			(slot-value monster 'direction-list))))
		;Heal one hp every ten turns
		(heal-naturally monster)
		)
	)

;This is just a helper function since this was 
;too gross to put into the code up there unshortened
;Parameters: 	a tuple of directions to see if we can move there
;Returns: 		true if it's not blocked, false if it is
(defun not-blocked (creature directions)
	;This line of code gets the tile the character is trying to move on to
	(defparameter *target-tile* (elt (elt *map* (+ (slot-value creature 'y-pos) (elt directions 0))) (+ (slot-value creature 'x-pos) (elt directions 1))))
	
	;This line of code checks to see if the square pointed to by directions is a wall
	(CHAR/= (slot-value *target-tile* 'symbol) #\#))

;shift-creature moves a creature to a new tile
;Parameters: 	The creature that's moving, a list detailing the direction it's moving in
;Returns: 		nothing interesting
(defun shift-creature (creature directions)
	;Figure out which tile we're trying to move to.
	(defparameter *target-tile* (elt (elt *map* (+ (slot-value creature 'y-pos) (elt directions 0))) (+ (slot-value creature 'x-pos) (elt directions 1))))
	;set the current tile's character value to nothing
	(setf (slot-value (slot-value creature 'tile) 'character) nil)
	;update the character's y and x positions
	(setf (slot-value creature 'y-pos) (+ (slot-value creature 'y-pos) (elt directions 0)))
    (setf (slot-value creature 'x-pos) (+ (slot-value creature 'x-pos) (elt directions 1)))
	;set the creature's tile value to the tile we're moving to
	(setf (slot-value creature 'tile) *target-tile*)
	;set the new tile's character value to the character
	(setf (slot-value *target-tile* 'character) creature))

;move-in-direction checks if a creature can move somewhere,
;and if the move they want to make should result in an attack
;Parameters: 	The creature that's moving, a list detailing the direction it's moving in
;Returns: 		nothing interesting
(defun move-in-direction (creature directions)
    ;if the tile we're moving to isn't a wall, we move our player to there
	(if(not-blocked creature directions)
		;If the tile doesn't have a character in it, move there
    	(if (not(slot-value *target-tile* 'character)) 
    		(shift-creature creature directions)
    		;otherwise, attack that character instead
    		(attack creature (slot-value *target-tile* 'character)))))

;attack makes one creature to attack another
;Parameters: 	The attack creature and the defending creature
;Returns: 		Nothing interesting
(defun attack (attacker defender)
	;Print what's happening to the log
	(log-message (format nil "~a attacks ~a." (slot-value attacker 'name) (slot-value defender 'name)))
	;Reduce the defender's hp by the power of the attacker
	(setf (slot-value defender 'currenthp) (- (slot-value defender 'currenthp) (slot-value attacker 'power)))
	;If the defender has less than 0 health, they're dead
	(when(<= (slot-value defender 'currenthp) 0)(die defender)
		;increment the attacker's xp by one
		(setf (slot-value attacker 'xp) (+ (slot-value attacker 'xp) 1)))
	)

;die removes a character from the map
;Parameters: 	The creature that's dying
;Returns: 		Nothing interesting
(defun die (departed)
	;update the log to reflect that a character is dead
	(setf *log* (append *log* (list (format nil "~a has died." (slot-value departed 'name)))))
	;If the player's dead, we're done, so we set our *done* variable 
	;to true so the game knows to quit the main loop
	(if (eq departed *player*) (setf *done* t))
	;Otherwise the dead character is a monster, 
	;and we should remove it from the current list of monsters
	(setf *monsters* (remove departed *monsters*))
	;For posterity we should also remove it from the list that exists within the dungeon list
	(setf (slot-value (elt *dungeon* (slot-value *player* 'current-level)) 'monsters) *monsters*)
	;Set the character value of the tile the character was standing on to nothing
	(setf (slot-value (slot-value departed 'tile) 'character) nil))

;log-message appends a string to our log, which will be printed 
;to the screen during the next iteration of the display loop
;Parameters: 	A string to append to the log.
;Returns: 		Nothing interesting
(defun log-message (input)
(setf *log* (append *log* (list input))))

;Class Definitions

;the class that currently encompasses all creatures
(defclass creature () 
	;the different fields of the creature
	(maxhp 		;Character's maximum hp (generally doesn't change)
	currenthp 	;Character's current hp (changes constantly)
	
	;Character's x and y position
	(y-pos
		:initarg :y-pos)
	(x-pos 
		:initarg :x-pos) 
	name
	symbol 		;The symbol that represents the character on the map
	(power 		;The amount of damage the character deals in combat
		:initform 1) ;Generally initialized at 0
	
	(xp 		;The amount of experience points the character has earned
		:initform 0) ; always starts at 0

	tile 		;The tile the character is standing on
	
	(can-see-player 	;a boolean value that tells whether or not a monster
		:initform nil) 	;can currently see the player character (defaults to false)
						
	
	(regen-counter   	;A counter that keeps track of how close the
		:initform 0)  	;character currently is to healing one hp
	
	(current-level 		;Keeps track of what level the character is currently on
		:initform 0)  	;The player starts on level 0, and the monsters don't 
						;currently use this value, so it inits to 0
	
	;A list of tiles the character can see (doesn't actually wind up getting used, 
	;and keeping track of it wastes a lot of time, so I'm disabling it for now)				
	;(line-of-sight 			
	;	:initform (list)) 	
	
	;a list of directions that keeps track of where 
	;the character is planning on going next
	(direction-list
		:initform (list))
	
	;A list of items the character currently has
	(inventory
		:initform (list))))

;A simple class for representing items
(defclass item ()
	;Items have a symbol that represents them on the map.
	((symbol
		:initarg :symbol)
	;And they have a name
	(name
		:initarg :name)))	

;Our class for the objects that 
;compose each space on the map
(defclass tile ()
	;They know their coordinates on the map
	((y-pos
		:initarg :y-pos) 
	(x-pos
		:initarg :x-pos)

	;They inherently display a certain symbol
	(symbol
		:initarg :symbol)
	
	;They know what character is standing on them.
	(character
		:initform nil) ;(Sensibly, this defaults to nobody)

	;While the shortest path to a plac is being determined,
	;Tiles can know how far they are from the origin square
	(path-value
		:initform 0)
	;A flag to show if they can currently be seen by the player
	(visible
		:initform nil)
	;A flag to show if the player can remember seeing them ever
	(explored
		:initform nil)
	;A list of items on the tile
	(items
		:initform (list))))

;Our class of objects that keeps track of 
;information about each level of the dungeon
(defclass level ()
	;A map of the level (a vector of vectors of tile objects)
	(level-map
		:initarg :level-map
	;Their position in the dungeon 
	;(a lower number is closer to top of the dungeon)
	(number
		:initarg :number)
	;The coordinates of the level's staircases
	;(We keep track of these so we know where to put
	;characters who are traveling to different levels)
	down-stairs
	up-stairs
	;A list of monsters that exist on the level
	(monsters
		:initform (list))))

;Our init-level function creates a new level, prepares it, 
;and appropriately places it in the dungeon
;Parameters: 	None
;Returns: 		Nothing Interesting
(defun init-level ()
	;Create the new level, setting it's level number to 
	;the current length of the dungeon list
	;(This should make it match it's index in the dungeon)
	(defparameter *level* (make-instance 'level 
			:number (length *dungeon*)))

	;Create a new map by generating a new level that's 36 squares by 36 squares
	;Passing in the length of the dungeon so the level-generator can know whether
	;or not to put the player in the new map
	(defparameter *new-level-map* (string-to-vector-map(generate-level 36 36 (= (length *dungeon*) 0))))

	;Give the new level map to our new level
	(setf (slot-value *level* 'level-map) *new-level-map*)
	;Tell the new level where it's down and up staircases are
	(setf (slot-value *level* 'down-stairs) *down-stairs*)
	(setf (slot-value *level* 'up-stairs) *up-stairs*)
	;Add the level to the end of the dungeon list
	(setf *dungeon* (append *dungeon* (list *level*)))
	;Set the global map to the new level map briefly
	(setf *map* (slot-value *level* 'level-map))
	;Set the global list of monsters to an empty list
	(setf *monsters* (list))
	;Populate the new level with items and characters
	(unpack-object-stack)
	;Assign the new list of monsters to the new level
	(setf (slot-value *level* 'monsters) *monsters*))

;Init-player creates a new creature, sets its parameters 
;to those the player should have, 
;and then sets the global player variable to that creature
;Parameters 	y and x position to put the player at
;Returns 		nothing interesting
(defun init-player (y x)
	;Create the creature object
	(defparameter *player* (make-instance 'creature :y-pos y :x-pos x))
	;set it's name
	(setf (slot-value *player* 'name) "foobar")
	;set max and current hp
	(setf (slot-value *player* 'maxhp) 10)
	(setf (slot-value *player* 'currenthp) 10)
	;set it's symbol to '@'
	(setf (slot-value *player* 'symbol) #\@)
	;Update it's tile field
	(setf (slot-value *player* 'tile) (elt (elt *map* y) x))
	;Update the tile's character field
	(setf (slot-value (elt (elt *map* y) x)'character) *player*))
	
;Init-player creates a new creature, sets its parameters 
;to those a goblin should have, and puts it on the map
;Parameters 	y and x position to put the player at
;Returns 		nothing interesting
(defun init-goblin (y x)
	;create the new instance
	(defparameter *goblin* (make-instance 'creature :y-pos y :x-pos x))
	;set it's name
	(setf (slot-value *goblin* 'name) "Space goblin")
	;set current and max hp
	(setf (slot-value *goblin* 'maxhp) 5)
	(setf (slot-value *goblin* 'currenthp) 5)
	;set it's symbol to 'g'
	(setf (slot-value *goblin* 'symbol) #\g)
	;update the goblin's tile field
	(setf (slot-value *goblin* 'tile) (elt (elt *map* y) x))
	;update that tile's character field
	(setf (slot-value (elt (elt *map* y) x)'character) *goblin*)
	;Add the goblin to the current list of monsters
	(setf *monsters* (append *monsters* (list *goblin*))))

;print-creature puts a creature's symbol in 
;the appropriate color on the screen
;Parameters: 	The y and x positions on the screen, the creature
;Returns: 		Nothing interesting
(defun print-creature (creature y x)
	;All creatures are red right now
	(attrset :cred)
	;Print the character
	(mvaddch y x (slot-value creature 'symbol)) 
	;Reset the color to gray
	(attrset :cgray))

;heal-naturally updates a creature's regeneration counter,
;allowing that creature to heal 1 hp every ten turns
;Parameters: 	The creature to heal
;Returns: 		Nothing interesting
(defun heal-naturally (creature)
	;When the creature's hp is less than it's max, increment the regen counter
	(when (< (slot-value creature 'currenthp)(slot-value creature 'maxhp))
		(setf (slot-value creature 'regen-counter) (+ 1 (slot-value creature 'regen-counter)))
		;When the regen counter is at ten, heal the character and set the regen-counter back to 0
		(when (>= (slot-value creature 'regen-counter) 10)
			(setf (slot-value creature 'currenthp) (+ 1 (slot-value creature 'currenthp)))
			(setf (slot-value creature 'regen-counter) 0))))

;curses helper functions

;hline draws a horizontal line
;Parameters: 	the y and x positions to star the line at, 
; 				the symbol to draw the line with, and the length of the line
;Returns: 		Nothing interesting
(defun hline (y-start x-start symbol length)
	;Draws a number of characters equal to the length 
	;of the line to the right starting from the initial x position
	(loop for x from x-start to (+ x-start length) 
		do (mvaddch y-start x symbol)))

;hline draws a horizontal line
;Parameters: 	the y and x positions to star the line at, 
; 				the symbol to draw the line with, and the length of the line
;Returns: 		Nothing interesting
(defun vline (y-start x-start symbol length)
	;draws a number of characters equal to the length 
	;of the line down from the initial y position
	(loop for y from y-start to (+ y-start length) 
		do (mvaddch y x-start symbol)))

;Our list-adjacent-tiles function takes a tile,
;and gives back a list of all the tiles next to that tile
;Parameters: 	the tile whose neighbors we want
;Returns: 		a list of the tile's neighbors
(defun list-adjacent-tiles (center-tile)
	;Initialize our list of neighbor tiles
	(defparameter *adjacent-tiles* (list))

	;starting at the tile to the north-west of our current tile,
	;work our way over each tile next to the center tile
	(loop for y from (- (slot-value center-tile 'y-pos) 1) to (+ (slot-value center-tile 'y-pos) 1)
		do(loop for x from (- (slot-value center-tile 'x-pos) 1) to (+ (slot-value center-tile 'x-pos) 1)
			;Don't add the center tile to the list
			do(if (not(and (= y (slot-value center-tile 'y-pos)) (= x (slot-value center-tile 'x-pos))))
				;add this tile to the list of adjacent tiles 
				(setf *adjacent-tiles* (append *adjacent-tiles* (list (elt (elt *map* y) x)))))))
	;Return the list of adjacent tiles
	*adjacent-tiles*)

;clear-path-values goes through every tile on the 
;map and sets it's path-value to 0
;Parameters: 	None
;Returns: 		Nothing Interesting
(defun clear-path-values ()
	;For each tile
	(loop for row across *map*
		do(loop for tile across row
			;Set the path-value to 0
			do(setf (slot-value tile 'path-value) 0))))

;clear-path-values goes through every tile on the 
;map and sets it's visibility to false
;Parameters: 	None
;Returns: 		Nothing Interesting
(defun clear-visibility()
	;For each tile
	(loop for row across *map*
		do(loop for tile across row
			;reset it's visibility
			do(setf (slot-value tile 'visible) nil))))

;find-path finds the shortest route from a creature to a location on the map
;Parameters: 	the creature who wants to go somewhere, the tile they want to go to
;Returns:		nothing, but updates the creature's list of directions
(defun find-path (creature target)
	;Define a tile variable that will be moved around to find the path
	(defparameter *current-tile* (slot-value creature 'tile))
	;Define the tile the creature is on as the origin
	(defparameter *origin-tile* (slot-value creature 'tile))
	;Initialize a list of tiles that should be explored next
	(defparameter *unexplored-tiles* (list))
	;Reset all path-values
	(clear-path-values)

	;Traverse tiles until we land on the target tile
	(loop while (not(eq *current-tile* target))
			;For every tile adjacent to the current tile
			do(loop for tile in (list-adjacent-tiles *current-tile*)
				;If the tile we're looking at in't a wall, and it's path value is zero 
				;(indicating that we haven't gone over it already)
				do (when (and (char/= (slot-value tile 'symbol) #\#) (= (slot-value tile 'path-value) 0))
						;If we're just starting, we set all tiles adjacent to the origin to 1 
						;(this way we don't have to set the origin's path-value to 0, which would confuse the algorithm)
						(if (eq *current-tile* *origin-tile*)
							(setf (slot-value tile 'path-value) 1) 

							;If the current tile isn't the origin, 
							;and the tile we're looking at right now isn't either,
							(if (not(eq tile *origin-tile*)) 
								;set the path-value of the tile we're inspecting 
								;to the current tile's value + 1
								;(The tile we're looking at is one square further 
								;away from the player than the current one)
								(setf (slot-value tile 'path-value) (+ (slot-value *current-tile* 'path-value) 1))))
						;Add the tile we're looking at to the list of tiles 
						;which haven't been the *current-tile* yet
						(setf *unexplored-tiles* (append *unexplored-tiles* (list tile)))
						;If the tile we're looking at is the target tile, 
						;we break out of this part of the algorithm
						(if (eq tile target) 
							(return))))
			;If we haven't found the target tile, update the current tile to the first
			;tile in our list of unexplored tiles and start the loop again
			(when (and (> (length *unexplored-tiles*) 0) (not (eq *current-tile* target))) 
					;update the current tile
					(setf *current-tile* (elt *unexplored-tiles* 0))
					;Remove the tile that is now the current tile from our unexplored list
					(setf *unexplored-tiles* (remove *current-tile* *unexplored-tiles*))))
	
	;Work our way back from the target to the origin, 
	;updating the creature's list of directions as we go	
	(loop while (not(eq *current-tile* *origin-tile*))
		;for each tile adjacent to the current tile
		do (loop for tile in (list-adjacent-tiles *current-tile*)
			;if the tile we're looking at has a path-value one less 
			;than the current tile:
			do (when (and (eq (slot-value tile 'path-value) (- (slot-value *current-tile* 'path-value) 1)) 
				;if the ajacent tile's path-value isn't 0, set the current tile to that tile
				(or (eq tile *origin-tile*) (> (slot-value tile 'path-value) 0)))
				;add a set of directions to the creature's direction list
				(setf (slot-value creature 'direction-list) 
					(append
						;The  
						(list (list 
							;the y direction you have to travel in to get from the 
							;new current tile to the old one
							(- (slot-value *current-tile* 'y-pos) (slot-value tile 'y-pos)) 
							;the x direction you need to travel in
							(- (slot-value *current-tile* 'x-pos) (slot-value tile 'x-pos))))
					;append the new directions to the top of the direction-list
					(slot-value creature 'direction-list)))
				;set the *current-tile* to the new tile
				(setf *current-tile* tile)
				;Since we found a new tile we want to go to,
				;we can break out of the inner loop and look 
				;for the next tile to go to
				(return)))))

;get-line-of-sight updates a character's line of sight based on their position on the map
;Parameters: 	The character who's line of sight we're updating
;Returns: 		Nothing of interest, but has an effect on the object we pass it
(defun get-line-of-sight (character)

	;There's some disabled code in here that can be reactivated if we ever want
	;each character to maintain a list of all the tiles they can see
	;set the character's line of sight to a list containing only the tile they are standing on
	;(setf (slot-value character 'line-of-sight) (list (slot-value character 'tile)))
	
	;Assume the character can't see anything yet (so they can't see the player yet)
	(setf (slot-value character 'can-see-player) nil)
	;if the character is the player, clear each tile's visibility boolean
	(if (eq character *player*)
		(clear-visibility))
	
	;We can see 8 squares in any direction
	(defvar distance 8)

	;set a few variables that'll be used during the algorithm
	(defvar eps)
	(defvar y)
	(defvar tile)

	;for each direction we can trace a digital line in
	(loop for direction from 0 to 7 
		;for each possible slope (p/q)
		do (loop for q from 0 to distance
			do (loop for p from 0 to q
				;for each possible starting offset
				do (loop for eps-start from 0 to q 
					;y keeps track of how high our 
					;digital line goes on that axis
					do (setf y 0)
					;set our temporary eps value 
					;(we'll be incrementing that within the loop)
					(setf eps eps-start)
					;progressing forwards along the line
					(loop for x from 1 to distance
						;update our eps
						do (setf eps (+ eps p))
						;if it's greater than our rise, adjust y,
						;and readjust our eps's value
						(when (>= eps q)
							(setf eps (- eps q))
							;if we're going in direction 2, 3, 6, or 7, decrement y
							(if (find direction (list 2 3 6 7)) 
								(setf y (- y 1))
								;otherwise, increment it
								(setf y (+ y 1))))

						;for half of the directions we go in, we're drawing lines vertically as opposed to horizontally
						;(we're drawing them along the y axis instead of the x axis, 
						;so we add the x value to the y-pos on the map and vice versa)
						(if (< direction 4)
							;set a shorthand for the tile we're actually on
							;by setting our 'tile' variable to the tile we find
							;once we translate our coordinates along the digital line we're drawing
							;to actual coordinates on the map
							(setf tile 
								;find the actual tile we're currently on
								(elt (elt *map* (+ (slot-value character 'y-pos) y))
									(+ (slot-value character 'x-pos) 
								;if we're going in an odd direction, we're headed in the 
								;opposite direction along the axis we're traveling across, so we invert x
										(if (oddp direction) 
											(* x -1)
											;otherwise x is unmodified 
											x))))
							;if our direction is greater than four, we just swap our y coordinate with our x coordinate
							(setf tile
								;find the tile on the map 
								(elt (elt *map* (+ (slot-value character 'y-pos) 
									;if our direction is odd, invert x
									(if (oddp direction) 
										(* x -1)
										x))) 
									(+ (slot-value character 'x-pos) y))))

						;disabled code
						;(when (not (find tile (slot-value character 'line-of-sight)))
							;(push tile (slot-value character 'line-of-sight))
							;(nreverse (slot-value character 'line-of-sight))

							;if we're the player, we can see the tile we're currently on
							(when (eq character *player*) 
								(setf (slot-value tile 'visible) t)
								;once we've seen it once, we remember it
								(setf (slot-value tile 'explored) t))
							
							;if we're the monster, and the tile we're looking at has the player in it,
							;note that we can see the player
							(if (eq (slot-value tile 'character) *player*) 
								(setf (slot-value character 'can-see-player) t));)
						;if the tile we're currently on is a wall, we don't walk along this line any further
						(if (char= (slot-value tile 'symbol) #\#) (return))))))))
