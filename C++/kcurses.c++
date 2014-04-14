/////////////////////////////////////////////////////////////////////////////
//File name: kcurses.c++
//Author: Klemente Gilbert-Espada										
//Description: These are the functions I've written for management of the
//curses library (and translated into C++)
/////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>


/*
initialize color pairs so that the curses code refers to the color pair: 
COLOR with a black background. 

This compresses the code for writing colored
symbols, as using different backgrounds with our map looks very weird,
so we shouldn't expect to use different backgrounds often if at all.
*/
int initColorPairs(){
	init_pair(COLOR_RED,COLOR_RED, COLOR_BLACK);
	init_pair(COLOR_WHITE,COLOR_WHITE, COLOR_BLACK);
	init_pair(COLOR_RED,COLOR_RED,COLOR_BLACK);
	init_pair(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
	init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK);
	init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK);
	init_pair(COLOR_MAGENTA, COLOR_MAGENTA, COLOR_BLACK);
	init_pair(COLOR_YELLOW, COLOR_YELLOW, COLOR_BLACK);
	return 0;
}

/*
Our initCurses function sets up the curses window. (The standard screen)
*/
int initCurses(){
	//Initalize the screen
	initscr();
	//cbreak allows input to be recieved without the user needing to press
	//'enter' in between every keystroke
	cbreak();
	//noecho makes it so that input isn't printed to the screen when it's recieved
	noecho();
	//keypad allows the use of non-standard keys such as backspace and enter
	keypad(stdscr, TRUE);
	//curs_set(0) hides the curser's position
	curs_set(0);
	//start color allows us to have colors
	start_color();
	//initColorPairs sets up a series of basic color pairs
	initColorPairs();
	return 0;
}

/*This function is necessary for cleaning up the terminal once our program
is done running.*/
int cursesCleanup(){
	//return keyboard to normal operation
	nocbreak();
	//turn echo back on so people can see what they're typing into the terminal
	echo();
	//close our curses window
	endwin();
	return 0;
}

/*This function draws a box in our curses window
paramaters: y pos, x pos, height of box, width of box
returns nothing*/
int drawBox(int y, int x, int height, int width){
	//draw the top horizontal line 
	mvhline(y, x + 1, ACS_HLINE, width -1 );
	//draw the bottom horizontal line
	mvhline(height + y, x + 1, ACS_HLINE, width - 1);
	//draw the left vertical line
	mvvline(y + 1, x, ACS_VLINE, height - 1);
	//draw the right vertical line
	mvvline(y + 1, width + x, ACS_VLINE, height - 1);
	//draw the upper left corner
	mvaddch(y, x, ACS_ULCORNER);
	//draw the lower left corner
	mvaddch(height + y, x, ACS_LLCORNER);
	//draw the upper right corner
	mvaddch(y, width + x, ACS_URCORNER);
	//draw the lower right corner
	mvaddch(y + height, x + width, ACS_LRCORNER);
	return 0;
}
/*
this function works just like curses' "hline()" function,
except you can also designate a color for the line
paramaters: y pos, x pos, character to draw the line with,
	number of times to draw that character, color of that character,
	screen we're drawing the line on.
returns: nothing
*/
int hColoredLine(int y, int x, char character, int num, int color){
	int i;
	for(i = 0; i < num; i++){
		//draw the specified character in the specified color
		//turn on the color
		attron(COLOR_PAIR(color));
		mvaddch(y, x + i, character);
		//turn off the color
		attroff(COLOR_PAIR(color));
	}
	return 0;
}

/*
The main function is a temporary placeholder that shows a basic example
of each function defined here
*/
int main(){
	initCurses();
	drawBox(1, 1, 3, 3);
	hColoredLine(5, 5, '5', 5, 5);
	getch();
	cursesCleanup();
	return 0;
}
