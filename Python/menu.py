import curses, traceback

#This function is necessary for cleaning up the terminal once our program
#is done running.
def cursesCleanup():
	# restore normal keyboard mode
	curses.nocbreak()
	# restore keystroke echoing
	curses.echo()
	# required cleanup call
	curses.endwin()

def draw(c, screen):

	#if the input character is down,
	#we move the cursor down one if we can.
	if chr(c) == 's' or c == 66:
		if menu.cursor < 2:
			menu.cursor += 1

	#if the input character is up, 
	#we move the cursor up one if we can.
	if chr(c) == 'w' or c == 65: 
		if menu.cursor > 0:
			menu.cursor -= 1

	#if the input character is enter,
	#we return the name of the selection
	#we're highlighting
	if c == 10:
		return menu.entry[menu.cursor]

	#Title of the menu
	screen.addstr(0, 30, "Main Menu")

	#We start with i at 4 as an aesthetic choice,
	#increasing i here starts populating the menu further
	#down on the screen
	i = 4
	#we go through every entry on our menu, and display it 
	#on the screen
	for entry in menu.entry:
		#if the entry we're printing is the entry we're currently on,
		#display it with the colors that suggest that it's highlighted
		if menu.entry[menu.cursor] == entry:
			screen.addstr(i, 5, entry, curses.color_pair(1))
		#otherwise display the entry normally
		else:
			screen.addstr(i, 5, entry)
		#the next entry will be printed two rows down
		i += 2	

	screen.refresh()

class menu:
	cursor = 0
	entry = ["New Game", "Load Game", "Exit"]

try:
	menuScreen = curses.initscr()
	# turn off keystroke echo
	curses.noecho()
	# hide the cursor
	curses.curs_set(0)

	# keystrokes are honored immediately, rather than waiting for the
	# user to hit Enter
	curses.cbreak()
	# start color display (if it exists; could check with has_colors())
	curses.start_color()
	# set up a foreground/background color pair that we'll use for indicating selection
	curses.init_pair(1,curses.COLOR_RED,curses.COLOR_WHITE)
	# clear screen
	menuScreen.clear()
	menuScreen.refresh()
	
	c = 0
	while True:
		if draw(c, menuScreen) == "Exit":
			break
		c = menuScreen.getch()	
	#Return the terminal to working order.
	cursesCleanup()

except:
	cursesCleanup()
	traceback.print_exc()