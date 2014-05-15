(defparameter *cursorX* nil)
(defparameter *cursorY* nil)
(defparameter *mapString*  
" ####                 ###               
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

(defun stringToVectorMap (inputString)
	(defparameter *y* 0)
	(defparameter *x* 0)
	(defparameter *map* (make-array 50 :fill-pointer 0))
	(loop while(CHAR/= (elt inputString *x*) #\x) 
		do;(setf *x* 0)
		(defparameter *row* (make-array 50 :fill-pointer 0))
		(loop while(CHAR/= (elt inputString *x*) #\Newline) 
			do (vector-push (elt inputString *x*) *row*) (setf *x* (+ *x* 1)) (if(Char= (elt *mapString* *x*) #\x) (return)) )
		(setf *x* (+ *x* 1))
		;(print *row*)
		(vector-push *row* *map*)
		(setf *y* (+ *y* 1))))

(defun basic-main ()
	(initPlayer 10 10)
	(stringToVectorMap *mapString*)
	;(load "curses.lisp")
	;(in-package :curses)
	(connect-console)
	(getLoop)
	(endwin))

(defun stamp (square)
	(mvaddstr *cursorY* *cursorX* square)
	(setf *cursorX* (+ *cursorX* 1))
	)

(defun printRow(row)
	(loop for square in row do(stamp square))
	(setf *cursorY* (+ *cursorY* 1))
	(setf *cursorX* 0)
	)

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
	(mvaddstr (+ 8 y-start) (+ 16 (* 2 x-start)) "@") (attrset :cwhite))

(defun drawFrame (y-start x-start height width)
	(vline (+ y-start 1) x-start #\| (- height 1))
	(vline (+ y-start 1) (+ width x-start) #\| (- height 1))
	(hline (+ height y-start) x-start #\- width)
	(hline y-start x-start #\- width ))

(defun display()
	(erase)
	(drawFrame 0 0 18 36)
	(display-screen 1 1)
	(drawFrame 19 0 4 36)
	(drawFrame 0 38 18 23)
	(mvaddstr 1 39 "Name:")
	(mvaddstr 1 44 (slot-value *player* 'name))
	(mvaddstr 2 39 "hp:")
	(attrset :cgreen)
	(hline 2 42 #\= 9)
	(attrset :cwhite)
	(mvaddstr 2 53 "10/10")
	(mvaddstr 4 39 "XP:0")
	(mvaddstr 5 39 "Current Level: 0")
	(mvaddstr 6 39 "Inventory:")
	)


(defun getLoop ()
	(erase)

	(display)

	(loop
  		(case (curses-code-char (getch))
  			((#\q) (return))
  			((#\s #\2) (setf (slot-value *player* 'yPos) (+ (slot-value *player* 'yPos) 1)))
  			((#\w #\8) (setf (slot-value *player* 'yPos) (- (slot-value *player* 'yPos) 1)))
  			((#\d #\6) (setf (slot-value *player* 'xPos) (+ (slot-value *player* 'xPos) 1)))
  			((#\a #\4) (setf (slot-value *player* 'xPos) (- (slot-value *player* 'xPos) 1)))
  			((#\1) (setf (slot-value *player* 'yPos) (+ (slot-value *player* 'yPos) 1)) (setf (slot-value *player* 'xPos) (- (slot-value *player* 'xPos) 1)))
 			((#\3) (setf (slot-value *player* 'yPos) (+ (slot-value *player* 'yPos) 1)) (setf (slot-value *player* 'xPos) (+ (slot-value *player* 'xPos) 1)))
  			((#\7) (setf (slot-value *player* 'yPos) (- (slot-value *player* 'yPos) 1)) (setf (slot-value *player* 'xPos) (- (slot-value *player* 'xPos) 1)))
  			((#\9) (setf (slot-value *player* 'yPos) (- (slot-value *player* 'yPos) 1)) (setf (slot-value *player* 'xPos) (+ (slot-value *player* 'xPos) 1))))
    	(display)))


;Class Definitions

(defclass creature () 
	(maxhp
	currenthp
	xPos
	yPos
	name
	symbol
	))

;This is perhaps the least elegant way to do this imaginable
(defun initPlayer (y x)
	(defparameter *player* (make-instance 'creature))
	(setf (slot-value *player* 'name) "foobar")
	(setf (slot-value *player* 'maxhp) 10)
	(setf (slot-value *player* 'currenthp) 10)
	(setf (slot-value *player* 'yPos) y)
	(setf (slot-value *player* 'xPos) x)
	(setf (slot-value *player* 'symbol) "@")
	)

(defun hline (y-start x-start symbol length)
	(loop for x from x-start to (+ x-start length) do (mvaddch y-start x symbol))
	)

(defun vline (y-start x-start symbol length)
	(loop for y from y-start to (+ y-start length) do (mvaddch y x-start symbol))
	)