#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>

#include "characters.c++"
#include "kcurses.c++"
#include "display.c++"


int main(){
	initCurses();
	mainDisplay();
	getch();
	cursesCleanup();
}