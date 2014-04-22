#include <iostream>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
#include <list>
using namespace std;

#include "characters.c++"

Player player(0, 0);

#include "mapObjects.c++"
#include "kcurses.c++"
#include "display.c++"

int main(){
	initCurses();
	Level levelOne = Level(rawMap);
	mainDisplay(levelOne);
	getch();
	cursesCleanup();
	std:: cout << player.xPos << endl;
}