##########################################################################
#File name: kcurses.py
#Author: Klemente Gilbert-Espada										
#Description: These are the functions I've written for management of the
#curses library
##########################################################################
import curses
# # # # # # # # # # # # # # CURSES FUNCTIONS # # # # # # # # # # # # # # # #

#This function is necessary for cleaning up the terminal once our program
#is done running.
def cursesCleanup():
	# restore normal keyboard mode
	curses.nocbreak()
	# restore keystroke echoing
	curses.echo()
	# required cleanup call
	curses.endwin()

#This function draws a box in our curses window
#paramaters: y pos, x pos, height of box, width of box,
#screen we're drawing the box on.
#returns nothing
def drawBox(y, x, height, width, screen):
	screen.hline(y, x + 1, curses.ACS_HLINE, width -1 )
	screen.hline(height + y, x + 1, curses.ACS_HLINE, width - 1)
	screen.vline(y + 1, x, curses.ACS_VLINE, height - 1)
	screen.vline(y + 1, width + x, curses.ACS_VLINE, height - 1)
	
	screen.addch(y, x, curses.ACS_ULCORNER)
	screen.addch(height + y, x, curses.ACS_LLCORNER)
	screen.addch(y, width + x, curses.ACS_URCORNER)
	screen.addch(y + height, x + width, curses.ACS_LRCORNER)

#this function works just like curses' "hline()" function,
#except you can also designate a color for the line
#paramaters: y pos, x pos, character to draw the line with,
#number of times to draw that character, color of that character,
#screen we're drawing the line on.
#returns nothing
def hColoredLine(y, x, char, num, color, screen):
	i = 0
	while i < num:
		#draw the specified character in the specified direction and color
		screen.addch(y, x + i, char, color)
		i += 1

def initCurses():
	mapScreen = curses.initscr()
	# turn off keystroke echo
	curses.noecho()
	# hide the cursor
	curses.curs_set(0)

	# keystrokes are honored immediately, rather than waiting for the
	# user to hit Enter
	curses.cbreak()
	# start color display
	curses.start_color()
	# initialize color pairs so that the curses code refers to the color pair: COLOR with a black background
	# this compresses the code for writing colored symbols, as using different backgrounds with our map looks very weird,
	# so we shouldn't expect to use different backgrounds often.
	curses.init_pair(curses.COLOR_WHITE,curses.COLOR_WHITE, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_RED,curses.COLOR_RED,curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_GREEN, curses.COLOR_GREEN, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_BLUE, curses.COLOR_BLUE, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_CYAN, curses.COLOR_CYAN, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_MAGENTA, curses.COLOR_MAGENTA, curses.COLOR_BLACK)
	curses.init_pair(curses.COLOR_YELLOW, curses.COLOR_YELLOW, curses.COLOR_BLACK)
	# clear screen
	mapScreen.clear()
	mapScreen.refresh()
	return mapScreen